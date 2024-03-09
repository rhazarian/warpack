use clap::{clap_app, crate_authors, crate_version};

fn main() {
    dotenv::dotenv().ok();

    let matches = clap_app!(Warpack =>
        (version: crate_version!())
        (author: crate_authors!())
        (about: "Warpack is a build tool, script compiler and map preprocessor for Warcraft III Lua maps.")
        (@subcommand build =>
            (about: "Uses the build.lua file in the current directory to build a map.")
            (setting: clap::AppSettings::TrailingVarArg)
            (@arg dir: --dir -d +takes_value "Sets the project directory.")
            (@arg BUILD_ARGS: ... "Arguments to pass to the build script.")
        )
        (@subcommand run =>
            (about: "Uses the build.lua file in the current directory to build and run a map.")
            (setting: clap::AppSettings::TrailingVarArg)
            (@arg dir: --dir -d +takes_value "Sets the project directory.")
            (@arg BUILD_ARGS: ... "Arguments to pass to the build script.")
        )
        (@subcommand exec =>
            (about: "Executes the specified lua file using Warpack runtime")
            (setting: clap::AppSettings::TrailingVarArg)
            (@arg script: +required +takes_value)
            (@arg BUILD_ARGS: ... "Arguments to pass to the build script.")
        )
    )
    .get_matches();

    std::process::exit(match run(matches) {
        Err(error) => {
            println!("[ERROR] An error has occured. Error chain:");
            println!("{}", error);

            let mut cause = error.source();
            while let Some(inner_cause) = cause {
                println!("{}", &inner_cause);
                cause = inner_cause.source();
            }

            1
        }
        Ok(_) => 0,
    });
}

fn run_build(arg: &clap::ArgMatches, mode: warpack_core::WarpackRunMode) -> Result<(), anyhow::Error> {
    let project_dir = arg
        .value_of("dir")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|| std::env::current_dir().unwrap());

    let script_args = arg
        .values_of("BUILD_ARGS")
        .map(std::iter::Iterator::collect)
        .unwrap_or_else(Vec::new);

    warpack_core::run_build_script(mode, project_dir, script_args)?;

    Ok(())
}

fn exec(arg: &clap::ArgMatches) -> Result<(), anyhow::Error> {
    let script = arg
        .value_of("script")
        .map(std::path::PathBuf::from)
        .unwrap();

    let script = std::fs::read_to_string(script)?;

    let script_args = arg
        .values_of("BUILD_ARGS")
        .map(std::iter::Iterator::collect)
        .unwrap_or_else(Vec::new);

    warpack_core::execute_script(warpack_core::WarpackRunMode::Build, script_args, |ctx| {
        ctx.load(&script).exec()?;

        Ok(())
    })?;

    Ok(())
}

fn run(matches: clap::ArgMatches) -> Result<(), anyhow::Error> {
    if let Some(arg) = matches.subcommand_matches("build") {
        run_build(arg, warpack_core::WarpackRunMode::Build)?;
    } else if let Some(arg) = matches.subcommand_matches("run") {
        run_build(arg, warpack_core::WarpackRunMode::RunMap)?;
    } else if let Some(arg) = matches.subcommand_matches("exec") {
        exec(arg)?;
    }

    Ok(())
}
