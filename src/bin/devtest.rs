use api_typer::*;
use polymorphio::FileOrStdout;
use std::{error::Error, path::PathBuf, process::exit};
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(short, long, help = "Silence all log messages")]
    quiet: bool,

    #[structopt(short, long, parse(from_occurrences), help = "Increase log output")]
    verbose: usize,

    #[structopt(
        short,
        long,
        parse(from_os_str),
        default_value = "-",
        help = "Output file"
    )]
    output: PathBuf,
}

fn app(opt: Opt) -> Result<(), Box<dyn Error>> {
    let t = test_data_spec();

    FileOrStdout::write_all(&opt.output, serde_yaml::to_string(&t).unwrap().as_bytes())?;

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

fn test_data_spec() -> ApiSpec {
    let define_custom = vec![(
        "ArcBit".to_string(),
        vec![
            ("rs".to_string(), "Arc<bool>".to_string()),
            ("ts".to_string(), "boolean".to_string()),
            ("elm".to_string(), "Bool".to_string()),
        ]
        .into_iter()
        .collect(),
    )]
    .into_iter()
    .collect();

    ApiSpec {
        module: "test".into(),
        define_custom,
        types: vec![
            TypeSpec::Enum {
                name: "TestEnum".into(),
                variants: vec![
                    EnumVariant {
                        name: "Foo".into(),
                        data: EnumVariantData::None,
                    },
                    EnumVariant {
                        name: "Bar".into(),
                        data: EnumVariantData::Single(ApiType::Basic(BasicApiType::Bool)),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![
                            EnumStructField {
                                name: "sub1".into(),
                                data: ApiType::Basic(BasicApiType::Uint),
                            },
                            EnumStructField {
                                name: "sub2".into(),
                                data: ApiType::Basic(BasicApiType::String),
                            },
                        ]),
                    },
                ],
            },
            TypeSpec::Struct {
                name: "MyStruct".into(),
                fields: vec![
                    StructField {
                        name: "num".into(),
                        data: ApiType::basic(BasicApiType::Int),
                    },
                    StructField {
                        name: "arr".into(),
                        data: ApiType::array(BasicApiType::String),
                    },
                    StructField {
                        name: "maybe".into(),
                        data: ApiType::option(BasicApiType::Float),
                    },
                    StructField {
                        name: "myenum".into(),
                        data: ApiType::basic(BasicApiType::Custom("TestEnum".into())),
                    },
                    StructField {
                        name: "arcbit".into(),
                        data: ApiType::basic(BasicApiType::Custom("ArcBit".into())),
                    },
                ],
            },
        ],
    }
}
