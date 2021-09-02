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
            BasicApiType::Recursive(s) => format!("Box<{}>", s),
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
            ApiType::Complex(ComplexApiType::Option(inner_type)) => {
                format!("Option<{}>", inner_type.to_rust())
            }
            ApiType::Complex(ComplexApiType::Array(inner_type)) => {
                format!("Vec<{}>", inner_type.to_rust())
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
        format!(
            "{indent}pub {}: {},\n",
            self.name,
            self.data.to_rust(),
            indent = INDENT,
        )
    }
}

impl RustTyper for EnumStructField {
    fn to_rust(&self) -> String {
        format!(
            "{indent}{indent}{}: {},\n",
            self.name,
            self.data.to_rust(),
            indent = INDENT,
        )
    }
}

impl RustTyper for EnumVariant {
    fn to_rust(&self) -> String {
        format!(
            "{indent}{}{},\n",
            self.name,
            self.data.to_rust(),
            indent = INDENT,
        )
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

                format!(
                    " {{\n{fields}{indent}}}",
                    fields = fields_fmt,
                    indent = INDENT
                )
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
    pub foo: u32,
    pub bar: String,
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
    pub foo: Vec<u32>,
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
    pub foo: Option<u32>,
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
    Foo,
    Bar,
    Qux,
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
    Foo,
    Bar(bool),
    Qux {
        sub1: u32,
        sub2: String,
    },
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
    Bar(Vec<u32>),
    Qux {
        sub1: Vec<bool>,
    },
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
    Bar(Option<u32>),
    Qux {
        sub1: Option<bool>,
    },
}";

        compare_strings(expected, create_spec_enum_with_option().to_rust());
    }

    fn create_spec_nested_option() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "x".into(),
                    data: ApiType::Complex(ComplexApiType::Option(Box::new(ApiType::option(
                        BasicApiType::Uint,
                    )))),
                }],
            }],
        }
    }

    #[test]
    fn rust_nested_option() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
    pub x: Option<Option<u32>>,
}";

        compare_strings(expected, create_spec_nested_option().to_rust());
    }

    fn create_spec_nested_array() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "x".into(),
                    data: ApiType::Complex(ComplexApiType::Array(Box::new(ApiType::array(
                        BasicApiType::Uint,
                    )))),
                }],
            }],
        }
    }

    #[test]
    fn rust_nested_array() {
        let expected = "\
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
    pub x: Vec<Vec<u32>>,
}";

        compare_strings(expected, create_spec_nested_array().to_rust());
    }
}
