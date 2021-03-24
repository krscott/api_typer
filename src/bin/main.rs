use api_typer::{ApiSpec, ElmTyper, RustTyper};
use polymorphio::{FileOrStdin, FileOrStdout};
use std::{error::Error, path::PathBuf, process::exit};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(short, long, help = "Silence all log messages")]
    quiet: bool,

    #[structopt(short, long, parse(from_occurrences), help = "Increase log output")]
    verbose: usize,

    #[structopt(parse(from_os_str), default_value = "-", help = "Input file")]
    input: PathBuf,

    #[structopt(
        short,
        long,
        parse(from_os_str),
        default_value = "-",
        help = "Output Elm File"
    )]
    elm: PathBuf,

    #[structopt(
        short,
        long,
        parse(from_os_str),
        default_value = "-",
        help = "Output Rust File"
    )]
    rust: PathBuf,
}

fn app(opt: Opt) -> Result<(), Box<dyn Error>> {
    let mut input_file = FileOrStdin::from_path(&opt.input)?;

    let spec: ApiSpec = serde_yaml::from_reader(input_file.lock())?;

    let elm_str = format!("-- Auto-generated by rust_elm_types\n\n{}\n", spec.to_elm());
    let rust_str = format!(
        "// Auto-generated by rust_elm_types\n\n{}\n",
        spec.to_rust()
    );

    FileOrStdout::write_all(&opt.elm, elm_str.as_bytes())?;
    FileOrStdout::write_all(&opt.rust, rust_str.as_bytes())?;

    Ok(())
}

fn main() {
    let opt = Opt::from_args();

    stderrlog::new()
        .module(module_path!())
        .quiet(opt.quiet)
        .verbosity(opt.verbose + 1)
        .init()
        .unwrap();

    match app(opt) {
        Ok(()) => {}
        Err(e) => {
            log::error!("Program exited: {}", e);
            exit(1);
        }
    }
}
