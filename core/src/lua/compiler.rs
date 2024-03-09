use std::path::PathBuf;

use rlua::prelude::*;

use crate::compiler;
use crate::compiler::TargetPlatform;
use crate::lua::macros;
use crate::lua::util::wrap_result;

pub fn get_compile_script_luafn(ctx: LuaContext) -> LuaFunction {
    ctx.create_function(|ctx, args: LuaTable| {
        let result = compile_script(ctx, args);

        Ok(wrap_result(ctx, result))
    })
    .unwrap()
}

fn compile_script(ctx: LuaContext, args: LuaTable) -> Result<String, anyhow::Error> {
    let src_directories: Vec<LuaString> = args.get("srcDirectories")?;
    let map_script: LuaString = args.get("mapScript")?;
    let target_platform_input: LuaString = args.get("platform")?;
    let target_platform_str = target_platform_input.to_str()?;

    let target_platform = if target_platform_str == "reforged" {
        Ok(TargetPlatform::Reforged)
    } else if target_platform_str == "ujapi" {
        Ok(TargetPlatform::UjAPI)
    } else {
        Err(anyhow::Error::msg(format!("unknown target platform '{}'", target_platform_str)))
    }?;

    let src_directories: Vec<PathBuf> = src_directories
        .iter()
        .map(|s| s.to_str().unwrap().into())
        .collect();

    let mut module_provider = compiler::ProjectModuleProvider::new(&src_directories);
    module_provider.scan();
    let macro_provider = macros::get_threadlocal_macro_provider();
    let mut compiler = compiler::ScriptCompiler::new(ctx, target_platform, module_provider, macro_provider)?;

    compiler.set_map_script(map_script.to_str()?.into());
    compiler.add_module("main", false, &Vec::default())?;
    compiler.add_module("config", true, &Vec::default())?;
    compiler.add_module("root", true, &Vec::default())?;

    Ok(compiler.emit_script()?)
}
