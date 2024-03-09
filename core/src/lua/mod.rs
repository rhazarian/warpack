use rlua::prelude::*;
use serde::{Deserialize, Serialize};

use crate::WarpackRunMode;

pub mod util;
pub mod compiler;
pub mod macros;
pub mod fs;
pub mod mpq;
pub mod launcher;
pub mod object;

#[derive(Serialize, Deserialize)]
struct ProjectLayout {
    #[serde(rename = "mapsDirectory")]
    maps_directory:   String,
    #[serde(rename = "srcDirectory")]
    src_directory:    String,
    #[serde(rename = "libDirectory")]
    lib_directory:    String,
    #[serde(rename = "targetDirectory")]
    target_directory: String,
}

pub fn setup_ceres_environ(ctx: LuaContext, run_mode: WarpackRunMode, script_args: Vec<String>) {
    const CERES_BUILDSCRIPT_LIB: &str = include_str!("../resource/buildscript_lib.lua");

    let globals = ctx.globals();

    let ceres_table = ctx.create_table().unwrap();

    ceres_table
        .set("registerMacro", macros::get_register_luafn(ctx))
        .unwrap();
    ceres_table
        .set("compileScript", compiler::get_compile_script_luafn(ctx))
        .unwrap();

    ceres_table
        .set(
            "runMode",
            ctx.create_function(move |ctx, _: ()| match run_mode {
                WarpackRunMode::RunMap => Ok(ctx.create_string("run")),
                WarpackRunMode::Build => Ok(ctx.create_string("build")),
                WarpackRunMode::LiveReload => Ok(ctx.create_string("reload")),
            })
            .unwrap(),
        )
        .unwrap();

    ceres_table
        .set(
            "getScriptArgs",
            ctx.create_function(move |_, _: ()| Ok(script_args.clone()))
                .unwrap(),
        )
        .unwrap();

    ceres_table
        .set("runWarcraft", launcher::get_runmap_luafn(ctx))
        .unwrap();

    let fs_table = fs::get_fs_module(ctx);
    let mpq_table = mpq::get_mpq_module(ctx);
    let object_table = object::get_object_module(ctx);

    globals.set("fs", fs_table).unwrap();
    globals.set("mpq", mpq_table).unwrap();
    globals.set("objdata", object_table).unwrap();
    globals.set("warpack", ceres_table).unwrap();

    ctx.load(CERES_BUILDSCRIPT_LIB)
        .set_name("buildscript_lib.lua")
        .unwrap()
        .exec()
        .unwrap();
}
