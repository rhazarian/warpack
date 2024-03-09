use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use std::thread_local;

use rlua::prelude::*;

use compiler::MacroProvider;

use crate::compiler;
use crate::error::*;
use crate::lua::util::evaluate_macro_args;

pub struct LuaMacroProvider {
    registered_macros: RefCell<HashMap<String, LuaRegistryKey>>,
}

impl LuaMacroProvider {
    fn register_macro<'lua>(&self, ctx: LuaContext<'lua>, id: &str, func: LuaFunction<'lua>) {
        let registry_key = ctx.create_registry_value(func).unwrap();

        self.registered_macros
            .borrow_mut()
            .insert(id.into(), registry_key);
    }
}

impl MacroProvider for LuaMacroProvider {
    fn is_macro_id(&self, id: &str) -> bool {
        self.registered_macros.borrow().contains_key(id)
    }

    fn handle_macro<'lua>(
        &self,
        ctx: LuaContext<'lua>,
        env: LuaTable<'lua>,
        id: &str,
        macro_invocation: compiler::MacroInvocation,
    ) -> Result<(String, LuaMultiValue<'lua>), MacroInvocationError> {
        let args = evaluate_macro_args(ctx, env, macro_invocation.args)?;
        let callback: LuaFunction = {
            let registered_macros = self.registered_macros.borrow();

            let registry_key = registered_macros.get(id).unwrap();
            ctx.registry_value(registry_key).unwrap()
        };

        let value = callback.call::<_, LuaValue>(args)?;

        let src = if let LuaValue::String(value) = value {
            String::from(value.to_str().unwrap())
        } else {
            String::from("")
        };

        let result = match ctx.load(src.as_str()).eval() {
            Ok(value) => value,
            _ => LuaMultiValue::from_vec(vec![LuaNil]),
        };

        Ok((src, result))
    }
}

impl MacroProvider for Rc<LuaMacroProvider> {
    fn is_macro_id(&self, id: &str) -> bool {
        (self.deref()).is_macro_id(id)
    }

    fn handle_macro<'lua>(
        &self,
        ctx: LuaContext<'lua>,
        env: LuaTable<'lua>,
        id: &str,
        macro_invocation: compiler::MacroInvocation,
    ) -> Result<(String, LuaMultiValue<'lua>), MacroInvocationError> {
        (self.deref()).handle_macro(ctx, env, id, macro_invocation)
    }
}

thread_local! {
    static LUA_MACRO_PROVIDER: RefCell<Option<Rc<LuaMacroProvider>>> = RefCell::new(None);
}

pub fn get_threadlocal_macro_provider() -> Rc<LuaMacroProvider> {
    LUA_MACRO_PROVIDER.with(|macro_provider| {
        let mut macro_provider = macro_provider.borrow_mut();

        if macro_provider.is_none() {
            let macro_provider_new = LuaMacroProvider {
                registered_macros: Default::default(),
            };

            macro_provider.replace(Rc::new(macro_provider_new));
        }

        Rc::clone(macro_provider.as_ref().unwrap())
    })
}

pub fn get_register_luafn(ctx: LuaContext) -> LuaFunction {
    ctx.create_function::<_, (), _>(|ctx, (id, callback): (String, LuaFunction)| {
        let lua_macro_provider = get_threadlocal_macro_provider();

        lua_macro_provider.register_macro(ctx, &id, callback);

        Ok(())
    })
    .unwrap()
}
