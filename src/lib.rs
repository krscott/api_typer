mod elm_typer;
mod parser;
mod rust_typer;
mod spec;
mod typescript_typer;

pub use spec::*;

pub use elm_typer::{to_elm, ElmTyper};
pub use rust_typer::{to_rust, RustTyper};
pub use typescript_typer::{to_typescript, TypescriptTyper};
