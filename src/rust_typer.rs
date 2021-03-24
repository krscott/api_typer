use crate::spec::*;

pub trait RustTyper {
    fn to_rust(&self) -> String;
}

const TYPE_DERIVE_HEADER: &str = "#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]";
const SERDE_ENUM_HEADER: &str = "#[serde(tag = \"var\", content = \"vardata\")]";

impl RustTyper for ApiSpec {
    fn to_rust(&self) -> String {
        self.types
            .iter()
            .map(|t| t.to_rust())
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

impl RustTyper for TypeSpec {
    fn to_rust(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_rust())
                    .collect::<Vec<_>>()
                    .join("");

                format!(
                    "\
{header}
pub struct {name} {{
{fields}}}",
                    header = TYPE_DERIVE_HEADER,
                    name = name,
                    fields = fields_fmt
                )
            }
            Self::Enum { name, variants } => {
                let variants_fmt = variants
                    .iter()
                    .map(|var| var.to_rust())
                    .collect::<Vec<_>>()
                    .join("");

                format!(
                    "\
{header}
{enum_header}
pub enum {name} {{
{variants}}}",
                    header = TYPE_DERIVE_HEADER,
                    enum_header = SERDE_ENUM_HEADER,
                    name = name,
                    variants = variants_fmt
                )
            }
        }
    }
}

impl RustTyper for StructField {
    fn to_rust(&self) -> String {
        format!("\tpub {}: {},\n", self.name, self.data.0)
    }
}

impl RustTyper for EnumStructField {
    fn to_rust(&self) -> String {
        format!("\t{}: {},\n", self.name, self.data.0)
    }
}

impl RustTyper for EnumVariant {
    fn to_rust(&self) -> String {
        format!("\t{}{},\n", self.name, self.data.to_rust())
    }
}

impl RustTyper for EnumVariantData {
    fn to_rust(&self) -> String {
        match self {
            Self::None => "".into(),
            Self::Single((rust_type, _)) => format!("({})", rust_type),
            Self::Struct(fields) => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_rust())
                    .collect::<Vec<_>>()
                    .join("");

                format!(" {{\n{fields}\t}}", fields = fields_fmt)
            }
        }
    }
}
