use crate::spec::*;

const TYPE_DERIVE_HEADER: &str =
    "#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]";
const SERDE_ENUM_HEADER: &str = "#[serde(tag = \"var\", content = \"vardata\")]";

pub fn to_rust(spec: &ApiSpec) -> String {
    spec.to_rust()
}

pub trait RustTyper {
    fn to_rust(&self) -> String;
}

impl RustTyper for BasicApiType {
    fn to_rust(&self) -> String {
        match self {
            BasicApiType::Custom(s) => s.clone(),
            BasicApiType::String => String::from("String"),
            BasicApiType::Int => String::from("i32"),
            BasicApiType::Uint => String::from("u32"),
            BasicApiType::Float => String::from("f32"),
            BasicApiType::Double => String::from("f64"),
            BasicApiType::Bool => String::from("bool"),
        }
    }
}

impl RustTyper for ApiType {
    fn to_rust(&self) -> String {
        match self {
            ApiType::Basic(basic_type) => basic_type.to_rust(),
            ApiType::Complex(ComplexApiType::Option(basic_type)) => {
                format!("Option<{}>", basic_type.to_rust())
            }
            ApiType::Complex(ComplexApiType::Array(basic_type)) => {
                format!("Vec<{}>", basic_type.to_rust())
            }
        }
    }
}

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
                    {header}\n\
                    pub struct {name} {{\n\
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
                    {header}\n\
                    {enum_header}\n\
                    pub enum {name} {{\n\
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
        format!("\tpub {}: {},\n", self.name, self.data.to_rust())
    }
}

impl RustTyper for EnumStructField {
    fn to_rust(&self) -> String {
        format!("\t\t{}: {},\n", self.name, self.data.to_rust())
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
            Self::Single(api_type) => format!("({})", api_type.to_rust()),
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

#[cfg(test)]
mod tests {
    use super::*;

    fn compare_strings(expected: &str, actual: String) {
        eprintln!(
            "============\n  Expected\n============\n\n{}\n\n\
            ==========\n  Actual\n==========\n\n{}\n\n",
            expected, actual
        );
        assert_eq!(expected, actual);
    }

    #[test]
    fn rust_empty() {
        let spec = ApiSpec {
            module: "TestType".into(),
            types: vec![],
        };

        compare_strings("", spec.to_rust());
    }

    fn create_spec_struct_simple() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![
                    StructField {
                        name: "foo".into(),
                        data: ApiType::Basic(BasicApiType::Uint),
                    },
                    StructField {
                        name: "bar".into(),
                        data: ApiType::Basic(BasicApiType::String),
                    },
                ],
            }],
        }
    }

    #[test]
    fn rust_struct_simple() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
\tpub foo: u32,
\tpub bar: String,
}";

        compare_strings(expected, create_spec_struct_simple().to_rust());
    }

    fn create_spec_struct_with_vec() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "foo".into(),
                    data: ApiType::array(BasicApiType::Uint),
                }],
            }],
        }
    }

    #[test]
    fn rust_struct_with_vec() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
\tpub foo: Vec<u32>,
}";

        compare_strings(expected, create_spec_struct_with_vec().to_rust());
    }

    fn create_spec_struct_with_option() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "foo".into(),
                    data: ApiType::option(BasicApiType::Uint),
                }],
            }],
        }
    }

    #[test]
    fn rust_struct_with_option() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
\tpub foo: Option<u32>,
}";

        compare_strings(expected, create_spec_struct_with_option().to_rust());
    }

    fn create_spec_enum_simple() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Enum {
                name: "TestEnum".into(),
                variants: vec![
                    EnumVariant {
                        name: "Foo".into(),
                        data: EnumVariantData::None,
                    },
                    EnumVariant {
                        name: "Bar".into(),
                        data: EnumVariantData::None,
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::None,
                    },
                ],
            }],
        }
    }

    #[test]
    fn rust_enum_simple() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
\tFoo,
\tBar,
\tQux,
}";

        compare_strings(expected, create_spec_enum_simple().to_rust());
    }

    fn create_spec_enum_complex() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Enum {
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
            }],
        }
    }

    #[test]
    fn rust_enum_complex() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
\tFoo,
\tBar(bool),
\tQux {
\t\tsub1: u32,
\t\tsub2: String,
\t},
}";

        compare_strings(expected, create_spec_enum_complex().to_rust());
    }

    fn create_spec_enum_with_vec() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Enum {
                name: "TestEnum".into(),
                variants: vec![
                    EnumVariant {
                        name: "Bar".into(),
                        data: EnumVariantData::Single(ApiType::array(BasicApiType::Uint)),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![EnumStructField {
                            name: "sub1".into(),
                            data: ApiType::array(BasicApiType::Bool),
                        }]),
                    },
                ],
            }],
        }
    }

    #[test]
    fn rust_enum_with_vec() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
\tBar(Vec<u32>),
\tQux {
\t\tsub1: Vec<bool>,
\t},
}";

        compare_strings(expected, create_spec_enum_with_vec().to_rust());
    }

    fn create_spec_enum_with_option() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Enum {
                name: "TestEnum".into(),
                variants: vec![
                    EnumVariant {
                        name: "Bar".into(),
                        data: EnumVariantData::Single(ApiType::option(BasicApiType::Uint)),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![EnumStructField {
                            name: "sub1".into(),
                            data: ApiType::option(BasicApiType::Bool),
                        }]),
                    },
                ],
            }],
        }
    }

    #[test]
    fn rust_enum_with_option() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
\tBar(Option<u32>),
\tQux {
\t\tsub1: Option<bool>,
\t},
}";

        compare_strings(expected, create_spec_enum_with_option().to_rust());
    }
}
