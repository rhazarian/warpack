use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::path::Component;
use std::rc::Rc;
use std::time::{Instant, Duration};

use indexmap::IndexMap;
use itertools::Itertools;
use pest::iterators::Pair;
use pest::Parser;
use rlua::prelude::*;
use walkdir::WalkDir;

use warpack_lua_parser as lua_parser;
use warpack_lua_parser::LuaParser;

use crate::error::*;
use crate::lua::util::evaluate_macro_args;
use crate::lua::util::lvalue_to_str;
use rlua::{Error, Table};

pub trait ModuleProvider {
    fn module_src(&self, module_name: &str) -> Option<String>;

    fn module_path(&self, module_name: &str) -> Option<PathBuf>;
}

pub struct ProjectModuleProvider {
    directories: Vec<PathBuf>,

    known_modules: HashMap<String, PathBuf>,
}

impl ProjectModuleProvider {
    pub fn new(directories: &[PathBuf]) -> ProjectModuleProvider {
        ProjectModuleProvider {
            directories: directories.into(),

            known_modules: Default::default(),
        }
    }

    pub fn scan(&mut self) {
        for dir in &self.directories {
            Self::scan_dir(&mut self.known_modules, &dir);
        }
    }

    fn scan_dir<P: AsRef<Path>>(modules: &mut HashMap<String, PathBuf>, path: P) {
        let path = path.as_ref();
        for entry in WalkDir::new(path).follow_links(true) {
            let entry = entry.unwrap();

            let ext = entry.path().extension();
            if ext.is_some() && ext.unwrap() == "lua" {
                let relative_path = entry.path().strip_prefix(path).unwrap();
                let module_path = relative_path
                    .components()
                    .filter_map(|s| {
                        if let Component::Normal(s) = s {
                            s.to_str()
                        } else {
                            None
                        }
                    })
                    .join(".");

                let module_path = &module_path[..(module_path.len() - 4)];

                modules.insert(module_path.into(), entry.into_path());
            }
        }
    }
}

impl ModuleProvider for ProjectModuleProvider {
    fn module_src(&self, module_name: &str) -> Option<String> {
        let path = self.known_modules.get(module_name);

        path.and_then(|s| fs::read_to_string(s).ok())
    }

    fn module_path(&self, module_name: &str) -> Option<PathBuf> {
        self.known_modules.get(module_name).cloned()
    }
}

pub trait MacroProvider {
    fn is_macro_id(&self, id: &str) -> bool;

    fn handle_macro<'lua>(
        &self,
        ctx: LuaContext<'lua>,
        env: LuaTable<'lua>,
        id: &str,
        macro_invocation: MacroInvocation,
    ) -> Result<(String, LuaMultiValue<'lua>), MacroInvocationError>;
}

#[derive(Debug)]
pub struct CompilationData {
    pub(crate) name: String,
    pub(crate) src: String,
}

#[derive(Debug)]
pub struct CompiledModule<'lua> {
    pub(crate) name: String,
    pub(crate) src: String,

    elapsed: Duration,

    exports: LuaMultiValue<'lua>,
}

#[derive(Debug)]
pub struct MacroInvocation<'src> {
    pub(crate) id: &'src str,
    pub(crate) args: Pair<'src, warpack_lua_parser::Rule>,
    pub(crate) span_start: usize,
    pub(crate) span_end: usize,
}

struct LuaEnvironment<'lua> {
    locals: HashSet<String>,
    table: Table<'lua>,
    parent: Option<Rc<RefCell<LuaEnvironment<'lua>>>>
}

impl<'lua> LuaEnvironment<'lua> {
    pub fn global(ctx: LuaContext<'lua>) -> Result<Self, Error> {
        Ok(LuaEnvironment {
            locals: HashSet::new(),
            table: ctx.globals(),
            parent: None
        })
    }

    pub fn new(ctx: LuaContext<'lua>, parent: Rc<RefCell<LuaEnvironment<'lua>>>) -> Result<Self, Error> {
        let metatable = ctx.create_table()?;
        metatable.set("__index", (*parent).borrow_mut().table.clone())?;
        metatable.set("__newindex", (*parent).borrow_mut().table.clone())?;
        let table = ctx.create_table()?;
        table.set_metatable(Some(metatable));
        Ok(LuaEnvironment {
            locals: HashSet::new(),
            table,
            parent: Some(parent)
        })
    }

    pub fn as_table(&self) -> Table<'lua> {
        self.table.clone()
    }

    pub fn declare_local(&mut self, name: String) {
        self.locals.insert(name);
    }

    pub fn set<V: ToLua<'lua>>(&mut self, name: &str, value: V) -> Result<(), Error> {
        if let Some(parent) = self.parent.clone() {
            if !self.locals.contains(name) {
                return (*parent).borrow_mut().set(name, value)
            }
        }
        self.table.raw_set(name, value)
    }
}

pub enum TargetPlatform {
    Reforged,
    UjAPI,
}

pub struct ScriptCompiler<'lua, MO: ModuleProvider, MA: MacroProvider> {
    pub(crate) ctx: LuaContext<'lua>,
    global_env: Rc<RefCell<LuaEnvironment<'lua>>>,

    map_script: Option<String>,
    target_platform: TargetPlatform,

    // map of modules that have already been compiled
    compiled_modules: IndexMap<String, CompiledModule<'lua>>,
    // set of modules that are currently in compilation
    compiling_modules: HashSet<String>,

    postcompile_data: IndexMap<usize, LuaFunction<'lua>>,

    module_provider: MO,
    macro_provider: MA,
}

impl<'lua, MO: ModuleProvider, MA: MacroProvider> ScriptCompiler<'lua, MO, MA> {
    pub fn new(
        ctx: LuaContext<'lua>,
        target_platform: TargetPlatform,
        module_provider: MO,
        macro_provider: MA,
    ) -> Result<ScriptCompiler<'lua, MO, MA>, Error> {
        Ok(ScriptCompiler {
            ctx,
            global_env: Rc::new(RefCell::new(LuaEnvironment::global(ctx)?)),

            map_script: None,
            target_platform,

            compiled_modules: Default::default(),
            compiling_modules: Default::default(),

            postcompile_data: Default::default(),

            module_provider,
            macro_provider,
        })
    }

    pub fn emit_script(&self) -> Result<String, CompilerError> {
        const SCRIPT_HEADER: &str = include_str!("resource/map_header.lua");
        const SCRIPT_FOOTER: &str = include_str!("resource/map_footer.lua");

        let mut out = String::new();

        out += SCRIPT_HEADER.trim();
        out += "\n\n";

        if let Some(map_script) = &self.map_script {
            out += "--[[ map script start ]]\n";
            out += map_script.trim();
            out += "\n--[[ map script end ]]\n\n";
        }

        if self.postcompile_data.len() > 0 {
            out += "warpack.data = {};\n";
        }
        for (idx, func) in self.postcompile_data.iter() {
            let values = func.call::<_, LuaMultiValue>(())?.into_iter().map(|v| lvalue_to_str(v));

            let value_src = if values.clone().any(|v| v.is_some()) {
                values.map(|v| v.unwrap_or(String::from("nil"))).join(", ")
            } else {
                String::from("nil")
            };

            let src = format!(
                "warpack.data[{idx}] = {value_src};\n",
                idx = idx, value_src = value_src
            );

            out += &src;
        }

        for (id, compiled_module) in self.compiled_modules.iter() {
            let module_header_comment = format!("--[[ start of module \"{}\" ]]\n", id);
            let module_header = format!(
                r#"warpack.modules["{name}"] = {{initialized = false, cached = nil, source = [================["#,
                name = id
            );
            let module_source = format!("\n{}\n", compiled_module.src);
            let module_footer = "]================]}\n";
            let module_footer_comment = format!("--[[ end of module \"{}\" ]]\n\n", id);

            out += &module_header_comment;
            out += &module_header;
            out += &module_source;
            out += &module_footer;
            out += &module_footer_comment;
        }

        out += SCRIPT_FOOTER.trim();
        out += "\n";

        Ok(out)
    }

    /// tries to find and compile the given module by it's module name
    /// using the ModuleProvider
    pub fn add_module(&mut self, module_name: &str, optional: bool, trace: &Vec<String>) -> Result<(LuaMultiValue<'lua>, Duration), CompilerError> {
        let mut trace = trace.clone();
        trace.push(module_name.into());
        //println!("adding module {}", module_name);

        if self.compiling_modules.contains(module_name) {
            return Err(CompilerError::CyclicalDependency {
                module_name: module_name.into(),
                trace: trace.join(" -> ")
            });
        }

        if let Some(module) = self.compiled_modules.get(module_name) {
            //println!("module {} already compiled", module_name);
            return Ok((module.exports.clone(), Duration::default()));
        }

        let src = self.module_provider.module_src(module_name);

        if src.is_none() {
            return if optional {
                Ok((LuaMultiValue::from_vec(vec![LuaNil]), Duration::default()))
            } else {
                Err(CompilerError::ModuleNotFound {
                    module_name: module_name.into(),
                })
            }
        }

        let src = src.unwrap();

        self.compiling_modules.insert(module_name.into());
        //println!("compiling module {}", module_name);
        let compiled_module = self.compile_module(module_name, &trace, &src);

        if let Err(error) = compiled_module {
            return match &error {
                CompilerError::ModuleError { .. } => Err(error),
                _ => {
                    Err(CompilerError::ModuleError {
                        module_name: module_name.into(),
                        module_path: self.module_provider.module_path(module_name).unwrap(),
                        error: Box::new(error),
                    })
                }
            }
        }

        let compiled_module = compiled_module.unwrap();
        let elapsed = compiled_module.elapsed.clone();
        let exports = compiled_module.exports.clone();

        //println!("finishing compiling module {}", module_name);
        println!("finished compiling module '{}' in {} sec", module_name, elapsed.as_secs());
        self.compiling_modules.remove(module_name);
        self.compiled_modules
            .insert(module_name.into(), compiled_module);

        Ok((exports, elapsed))
    }

    pub fn set_map_script(&mut self, map_script: String) {
        self.map_script = Some(map_script);
    }

    fn map_macro_err<'src>(&self, pair: Pair<'src, lua_parser::Rule>) -> Box<dyn FnOnce(MacroInvocationError) -> CompilerError + 'src> {
        return Box::new(move |err| match err {
            MacroInvocationError::CompilerError { error } => error,
            _ => CompilerError::MacroError {
                error: Box::new(err),
                diagnostic: pest::error::Error::new_from_span(
                    pest::error::ErrorVariant::CustomError { message: "".into() },
                    pair.as_span(),
                ),
            },
        })
    }

    fn preprocess_block<'src>(
        &mut self,
        src: &str,
        im_src: &mut String,
        other_modules_compilation_time: &mut Duration,
        trace: &Vec<String>,
        block: Pair<'src, lua_parser::Rule>,
        env: Rc<RefCell<LuaEnvironment<'lua>>>
    ) -> Result<Option<LuaMultiValue<'lua>>, CompilerError> {
        for stmt in block.into_inner() {
            if stmt.as_rule() == lua_parser::Rule::StmtReturn {
                let mut srcs: Vec<String> = Vec::new();
                let mut returned: Vec<LuaValue<'lua>> = Vec::new();
                if let Some(exp_list) = stmt.into_inner().find(|i| i.as_rule() == lua_parser::Rule::ExpList) {
                    for exp in exp_list.into_inner().filter(|i| i.as_rule() == lua_parser::Rule::Exp) {
                        if let Some((src, values)) = self.compiletime_value(&trace, other_modules_compilation_time, src, (*env).borrow_mut().as_table(), exp.clone()).map_err(self.map_macro_err(exp))? {
                            for value in values {
                                returned.push(value);
                            }
                            srcs.push(src)
                        }
                    }
                }
                *im_src += "return ";
                *im_src += &srcs.join(", ");
                *im_src += ";";
                return Ok(Some(LuaMultiValue::from_vec(returned)));
            } else if let Some(stmt_do) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::StmtDo) {
                *im_src += "do ";
                if let Some(child_block) = stmt_do.into_inner().find(|i| i.as_rule() == lua_parser::Rule::Block) {
                    if let Some(returned) = self.preprocess_block(src, im_src, other_modules_compilation_time, trace, child_block, Rc::new(RefCell::new(LuaEnvironment::new(self.ctx, env.clone())?)))? {
                        *im_src += " end;";
                        // TODO: doesn't happen in TSTL code, but we actually need to print out the rest.
                        return Ok(Some(returned))
                    }
                }
                *im_src += " end;";
                continue;
            } else if let Some(stmt_func_call) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::StmtFuncCall) {
                if let Some(invocation) = self.macro_invocation(stmt_func_call) {
                    let (macro_src, _) = self.handle_macro(&trace, other_modules_compilation_time, src, env.deref().borrow_mut().as_table(), invocation)
                        .map_err(self.map_macro_err(stmt))?;
                    *im_src += macro_src.as_str();
                    *im_src += ";";
                    continue;
                } else {
                    let _ = self.ctx.load(stmt.as_str()).set_environment((*env).borrow_mut().as_table())?.exec();
                }
            } else if let Some(local_def) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::StmtLocalDef) {
                if let Some(ident_list) = local_def.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::IdentList) {
                    let idents: Vec<String> = ident_list.into_inner().filter_map(|i| if i.as_rule() == lua_parser::Rule::Ident {
                        Some(String::from(i.as_str()))
                    } else {
                        None
                    }).collect();
                    for ident in idents.iter() {
                        (*env).borrow_mut().declare_local(ident.clone());
                    }
                    if let Some(exp_list) = local_def.into_inner().find(|i| i.as_rule() == lua_parser::Rule::ExpList) {
                        let mut i = 0;
                        let mut srcs: Vec<String> = Vec::new();
                        for exp in exp_list.into_inner().filter(|i| i.as_rule() == lua_parser::Rule::Exp) {
                            let mut env = env.deref().borrow_mut();
                            if let Some((src, values)) = self.compiletime_value(trace, other_modules_compilation_time, src, env.as_table(), exp.clone()).map_err(self.map_macro_err(exp.clone()))? {
                                for value in values {
                                    if let Some(name) = idents.get(i) {
                                        env.set(name.as_str(), value)?;
                                    }
                                    i += 1;
                                }
                                srcs.push(src);
                            } else {
                                i += 1;
                                srcs.push(String::from(exp.as_str()));
                            }
                        }
                        *im_src += "local ";
                        *im_src += &idents.join(", ");
                        *im_src += " = ";
                        *im_src += &srcs.join(", ");
                        *im_src += ";";
                        continue;
                    }
                }
            } else if let Some(local_def) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::StmtLocalFuncDef) {
                if let Some(ident) = local_def.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::Ident) {
                    if let Some(func_body) = local_def.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::FuncBody) {
                        let mut env = env.deref().borrow_mut();
                        env.declare_local(String::from(ident.as_str()));
                        let function: LuaValue = self.ctx.load(format!("function {}", func_body.as_str()).as_str()).set_environment(env.as_table())?.eval()?;
                        env.set(ident.as_str(), function)?;
                    }
                }
            } else if let Some(stmt) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::StmtFuncDef) {
                if let Some(func_name) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::FuncName) {
                    let idents: Vec<String> = func_name.clone().into_inner().filter_map(|i| if i.as_rule() == lua_parser::Rule::Ident {
                        Some(String::from(i.as_str()))
                    } else {
                        None
                    }).collect();
                    if idents.len() == 1 {
                        if let Some(func_body) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::FuncBody) {
                            let mut env = env.deref().borrow_mut();
                            let function: LuaValue = self.ctx.load(format!("function {}", func_body.as_str()).as_str()).set_environment(env.as_table())?.eval()?;
                            env.set(idents[0].as_str(), function)?;
                        }
                    } else {
                        let _ = self.ctx.load(stmt.as_str()).set_environment((*env).borrow_mut().as_table())?.exec();
                    }
                }
            } else if let Some(stmt) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::StmtAssign) {
                if let Some(var_list) = stmt.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::VarList) {
                    let vars: Vec<Pair<lua_parser::Rule>> = var_list.clone().into_inner().filter_map(|i| if i.as_rule() == lua_parser::Rule::Var {
                        Some(i)
                    } else {
                        None
                    }).collect();
                    if let Some(exp_list) = stmt.into_inner().find(|i| i.as_rule() == lua_parser::Rule::ExpList) {
                        let mut i =0;
                        let mut srcs: Vec<String> = Vec::new();
                        for exp in exp_list.into_inner().filter(|i| i.as_rule() == lua_parser::Rule::Exp) {
                            let mut env = env.deref().borrow_mut();
                            if let Some((src, values)) = self.compiletime_value(trace, other_modules_compilation_time, src, env.as_table(), exp.clone()).map_err(self.map_macro_err(exp.clone()))? {
                                for value in values {
                                    if let Some(var) = vars.get(i) {
                                        let exps: Vec<Pair<lua_parser::Rule>> = var.clone().into_inner().collect();
                                        if exps.len() == 1 && exps[0].as_rule() == lua_parser::Rule::AtomicExp {
                                            let idents: Vec<String> = exps[0].clone().into_inner().filter_map(|i| if i.as_rule() == lua_parser::Rule::Ident {
                                                Some(String::from(i.as_str()))
                                            } else {
                                                None
                                            }).collect();
                                            if idents.len() == 1 {
                                                env.set(idents[0].as_str(), value)?;
                                                continue;
                                            }
                                        };
                                        env.set("__WARPACK_TMP", value)?;
                                        let _ = self.ctx.load(format!("{} = __WARPACK_TMP;", var.as_str()).as_str()).set_environment(env.as_table())?.exec();
                                        env.set("__WARPACK_TMP", LuaNil)?;
                                    }
                                    i += 1;
                                }
                                srcs.push(src);
                            } else {
                                if let Ok(values) = self.ctx.load(exp.as_str()).set_environment(env.as_table())?.eval::<LuaMultiValue>() {
                                    for value in values {
                                        if let Some(var) = vars.get(i) {
                                            env.set("__WARPACK_TMP", value)?;
                                            let _ = self.ctx.load(format!("{} = __WARPACK_TMP;", var.as_str()).as_str()).set_environment(env.as_table())?.exec();
                                            env.set("__WARPACK_TMP", LuaNil)?;
                                        }
                                        i += 1;
                                    }
                                } else {
                                    i += 1;
                                }
                                srcs.push(String::from(exp.as_str()));
                            }
                        }
                        *im_src += format!("{} = {};", var_list.as_str(), &srcs.join(", ")).as_str();
                        continue;
                    }
                }
            }
            *im_src += stmt.as_str();
            *im_src += ";";
        }

        Ok(None)
    }

    /// will compile a single module with the given module name and source,
    /// as well as all of it's transitive dependencies, while processing macros
    fn compile_module(
        &mut self,
        module_name: &str,
        trace: &Vec<String>,
        src: &str,
    ) -> Result<CompiledModule<'lua>, CompilerError> {
        let now = Instant::now();
        let mut other_modules_compilation_time = Duration::new(0, 0);

        let mut im_src = String::new();
        let env = Rc::new(RefCell::new(LuaEnvironment::new(self.ctx, self.global_env.clone())?));
        let mut exports: LuaMultiValue<'lua> = LuaMultiValue::default();

        for chunk in LuaParser::parse(lua_parser::Rule::Chunk, src)? {
            if let Some(block) = chunk.into_inner().find(|i| i.as_rule() == lua_parser::Rule::Block) {
                if let Some(returned) = self.preprocess_block(src, &mut im_src, &mut other_modules_compilation_time, trace, block, env.clone())? {
                    exports = returned
                }
            }
        }

        let mut compilation_data = CompilationData {
            name: module_name.into(),
            src: String::new(),
        };

        let mut next_pair_start = 0;
        let mut emitted_index = 0;

        let parsed = LuaParser::parse(lua_parser::Rule::Chunk, im_src.as_str())?;
        for pair in parsed.flatten() {
            // ignore any pairs that are inside a macro invocation
            if pair.as_span().start() < next_pair_start {
                continue;
            }

            if let Some(invocation) = self.macro_invocation(pair.clone()) {
                next_pair_start = invocation.span_end;

                compilation_data.src += &im_src[emitted_index..invocation.span_start];
                emitted_index = invocation.span_end;

                let (macro_src, _) = self.handle_macro(trace, &mut other_modules_compilation_time, im_src.as_str(), (*env).borrow_mut().as_table(), invocation)
                    .map_err(self.map_macro_err(pair))?;
                compilation_data.src += macro_src.as_str();
            }
        }

        if emitted_index < im_src.len() {
            compilation_data.src += &im_src[emitted_index..im_src.len()];
        }

        Ok(CompiledModule {
            name: compilation_data.name,
            src: compilation_data.src,

            elapsed: now.elapsed() - other_modules_compilation_time,

            exports,
        })
    }

    fn is_macro_id(&self, id: &str) -> bool {
        match id {
            "postcompile" | "compiletime" | "require" => true,
            id => self.macro_provider.is_macro_id(id),
        }
    }

    fn handle_macro(
        &mut self,
        trace: &Vec<String>,
        compensation: &mut Duration,
        src: &str,
        env: LuaTable<'lua>,
        macro_invocation: MacroInvocation,
    ) -> Result<(String, LuaMultiValue<'lua>), MacroInvocationError> {
        let id = macro_invocation.id;
        //println!("{} {} {}", macro_invocation.span_start, macro_invocation.span_end, src);
        let newline_count = src[macro_invocation.span_start..macro_invocation.span_end]
            .chars()
            .filter(|c| *c == '\n')
            .count();

        let (mut src, value) = match id {
            "require" => self.handle_macro_require(trace, compensation, env, macro_invocation)?,
            "compiletime" => self.handle_macro_compiletime(env, macro_invocation)?,
            "postcompile" => self.handle_macro_postcompile(env, macro_invocation)?,
            id => self.macro_provider.handle_macro(
                self.ctx,
                env,
                id,
                macro_invocation,
            )?,
        };

        src.push_str(&("\n").repeat(newline_count));

        Ok((src, value))
    }

    fn handle_macro_require(
        &mut self,
        trace: &Vec<String>,
        compensation: &mut Duration,
        env: LuaTable<'lua>,
        macro_invocation: MacroInvocation,
    ) -> Result<(String, LuaMultiValue<'lua>), MacroInvocationError> {
        let args = evaluate_macro_args(self.ctx, env, macro_invocation.args)?.into_vec();

        if args.is_empty() {
            return Err(MacroInvocationError::message(
                "Require macro requires at least one argument".into(),
            ));
        }

        let optional = if args.len() >= 2 {
            if let LuaValue::Boolean(optional) = &args[1] {
                *optional
            } else {
                false
            }
        } else {
            false
        };

        return if let LuaValue::String(module_name) = &args[0] {
            let module_name = module_name.to_str().unwrap();
            let (exports, elapsed) = self.add_module(module_name, optional, trace)?;
            *compensation += elapsed;
            Ok((
                if optional {
                    format!("require(\"{}\", true)", module_name)
                } else {
                    format!("require(\"{}\")", module_name)
                },
                exports
            ))
        } else {
            Err(MacroInvocationError::message(
                "Require macro's first argument must be a string".into(),
            ))
        }
    }

    fn handle_macro_compiletime(
        &mut self,
        env: LuaTable<'lua>,
        macro_invocation: MacroInvocation,
    ) -> Result<(String, LuaMultiValue<'lua>), MacroInvocationError> {
        let mut args = evaluate_macro_args(self.ctx, env, macro_invocation.args)?
            .into_vec();

        if args.len() > 1 || args.is_empty() {
            return Err(MacroInvocationError::message(
                "Compiletime macro must have exactly one argument".into(),
            ));
        }

        let arg = args.remove(0);

        let prevalue = if let LuaValue::Function(func) = arg {
            func.call::<_, LuaMultiValue>(())?
        } else {
            LuaMultiValue::from_vec(vec![arg])
        };
        let value = LuaMultiValue::from_vec(prevalue.into_iter().map(|v| {
            if let LuaValue::Table(t) = v.clone() {
                let converter: LuaValue = t.get("compiletime").ok().unwrap_or(LuaNil);
                if let LuaValue::Function(func) = converter {
                    return func.call::<LuaTable, LuaValue>(t).ok().unwrap_or(LuaNil);
                }
            }
            return v;
        }).collect());
        let values = value.clone().into_iter().map(|v| lvalue_to_str(v));

        Ok((
            if values.clone().any(|v| v.is_some()) {
                values.map(|v| v.unwrap_or(String::from("nil"))).join(", ")
            } else {
                String::from("")
            },
            value
        ))
    }

    fn handle_macro_postcompile(
        &mut self,
        env: LuaTable<'lua>,
        macro_invocation: MacroInvocation,
    ) -> Result<(String, LuaMultiValue<'lua>), MacroInvocationError> {
        let mut args = evaluate_macro_args(self.ctx, env, macro_invocation.args)?
            .into_vec();

        if args.len() > 1 || args.is_empty() {
            return Err(MacroInvocationError::message(
                "Postcompile macro must have exactly one argument".into(),
            ));
        }

        let arg = args.remove(0);

        return if let LuaValue::Function(func) = arg {
            let idx = self.postcompile_data.len() + 1;
            self.postcompile_data.insert(idx, func);
            Ok((
                format!("warpack.data[{}]", idx),
                LuaNil.to_lua_multi(self.ctx)?
            ))
        } else {
            Err(MacroInvocationError::message(
                "Postcompile macro's argument must be a function".into(),
            ))
        };
    }

    fn compiletime_value<'src>(&mut self, trace: &Vec<String>, compensation: &mut Duration, src: &str, env: LuaTable<'lua>, pair: Pair<'src, lua_parser::Rule>) -> Result<Option<(String, LuaMultiValue<'lua>)>, MacroInvocationError> {
        if pair.as_rule() != lua_parser::Rule::Exp {
            return Ok(None);
        }
        if let Some(exp_value) = pair.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::Value) {
            if let Some(invocation) = self.macro_invocation(exp_value.clone()) {
                return match self.handle_macro(&trace, compensation, &src, env, invocation) {
                    Ok(value) => Ok(Some(value)),
                    Err(err) => Err(err)
                };
            } else if let Some(var) = exp_value.clone().into_inner().find(|i| i.as_rule() == lua_parser::Rule::Var) {
                if var.to_string() == "TARGET_PLATFORM" && !exp_value.into_inner().any(|i| i.as_rule() == lua_parser::Rule::Call) {
                    let target_platform = match self.target_platform {
                        TargetPlatform::Reforged => "reforged",
                        TargetPlatform::UjAPI => "ujapi",
                    };
                    return Ok(Some((target_platform.to_string(), target_platform.to_lua_multi(self.ctx)?)))
                }
            }
        }
        match self.ctx.load(pair.as_str()).set_environment(env) {
            Ok(env) => match env.eval() {
                Ok(value) => return Ok(Some((String::from(pair.as_str()), value))),
                _ => {}
            },
            _ => {}
        };
        //println!("rhs {}", "not exp atom!");
        return Ok(None);
    }

    /// Will try to extract a macro invocation out of the given pair, returning `None` if it can't find one.
    fn macro_invocation<'src>(&self, pair: Pair<'src, lua_parser::Rule>) -> Option<MacroInvocation<'src>> {
        if pair.as_rule() != lua_parser::Rule::StmtFuncCall && pair.as_rule() != lua_parser::Rule::Value {
            return None;
        }

        let var = pair
            .clone()
            .into_inner()
            .find(|i| i.as_rule() == lua_parser::Rule::Var)?;

        // we want the var to consist only of a single ident
        // if it's anything more complex, then it's never a macro
        // i really wish i had a proper AST here

        let var = var.into_inner().collect::<Vec<_>>();

        if var.len() > 1 {
            return None;
        }

        let ident = var.into_iter().next()?.into_inner().next()?;

        if ident.as_rule() != lua_parser::Rule::Ident {
            return None;
        }

        let id = ident.as_str();

        if !self.is_macro_id(id) {
            return None;
        }

        let calls = pair
            .clone()
            .into_inner()
            .filter(|i| i.as_rule() == lua_parser::Rule::Call)
            .collect::<Vec<_>>();

        if calls.is_empty() {
            return None;
        }

        let call = calls.into_iter().next()?;
        let span_start = pair.as_span().start();
        let span_end = call.as_span().end();

        let simple_call = call
            .into_inner()
            .find(|i| i.as_rule() == lua_parser::Rule::SimpleCall)?;

        let args = simple_call.into_inner().next()?;

        Some(MacroInvocation {
            id,
            args,
            span_start,
            span_end,
        })
    }
}
