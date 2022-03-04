use crate::spec::*;

const LANG: &str = "py";

const IMPORTS: &str = r#"from __future__ import annotations
import typing
import typing_extensions
import pydantic"#;

pub fn to_python(spec: &ApiSpec) -> String {
    spec.to_python(spec)
}

pub trait PythonTyper {
    fn to_python(&self, spec: &ApiSpec) -> String;
}

impl PythonTyper for BasicApiType {
    fn to_python(&self, spec: &ApiSpec) -> String {
        match self {
            BasicApiType::Custom(s) => spec.get_custom_type(s, LANG),
            BasicApiType::Recursive(s) => s.clone(),
            BasicApiType::String => String::from("str"),
            BasicApiType::Int => String::from("int"),
            BasicApiType::Uint => String::from("int"),
            BasicApiType::Float => String::from("float"),
            BasicApiType::Double => String::from("float"),
            BasicApiType::Bool => String::from("bool"),
        }
    }
}

impl PythonTyper for ApiType {
    fn to_python(&self, spec: &ApiSpec) -> String {
        match self {
            ApiType::Basic(basic_type) => basic_type.to_python(spec),
            ApiType::Complex(ComplexApiType::Option(inner_type)) => {
                format!("typing.Optional[{}]", inner_type.to_python(spec))
            }
            ApiType::Complex(ComplexApiType::Array(inner_type)) => {
                format!("list[{}]", inner_type.to_python(spec))
            }
            ApiType::Complex(ComplexApiType::Map(key_type, value_type)) => {
                format!(
                    "dict[{}, {}]",
                    key_type.to_python(spec),
                    value_type.to_python(spec)
                )
            }
        }
    }
}

impl PythonTyper for ApiSpec {
    fn to_python(&self, spec: &ApiSpec) -> String {
        let types = self
            .types
            .iter()
            .map(|t| t.to_python(spec))
            .collect::<Vec<_>>()
            .join("\n\n");

        format!("{}\n\n{}", IMPORTS, types)
    }
}

impl PythonTyper for TypeSpec {
    fn to_python(&self, spec: &ApiSpec) -> String {
        match self {
            Self::Struct { name, fields } => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_python(spec))
                    .collect::<Vec<_>>()
                    .join("");

                format!(
                    "class {name}(pydantic.BaseModel):\n{fields}",
                    name = name,
                    fields = fields_fmt
                )
            }
            Self::Enum { name, variants } => {
                fn var_class_def(spec: &ApiSpec, name: &str, var: &EnumVariant) -> Vec<String> {
                    match &var.data {
                        EnumVariantData::None => vec![format!(
                            r#"class {enumname}{varname}(pydantic.BaseModel):
    var: typing.Literal["{varname}"] = "{varname}"
"#,
                            varname = var.name,
                            enumname = name
                        )],
                        EnumVariantData::Single(api_type) => vec![format!(
                            r#"class {enumname}{varname}(pydantic.BaseModel):
    var: typing.Literal["{varname}"] = "{varname}"
    vardata: {datatype}
"#,
                            varname = var.name,
                            enumname = name,
                            datatype = api_type.to_python(spec)
                        )],
                        EnumVariantData::Struct(fields) => {
                            let sub_type_name = format!("{}{}Data", name, var.name);

                            let fields_fmt = fields
                                .iter()
                                .map(|field| field.to_python(spec))
                                .collect::<Vec<_>>()
                                .join("");

                            vec![
                                format!(
                                    "class {name}(pydantic.BaseModel):\n{fields}",
                                    name = sub_type_name,
                                    fields = fields_fmt
                                ),
                                format!(
                                    r#"class {enumname}{varname}(pydantic.BaseModel):
    var: typing.Literal["{varname}"] = "{varname}"
    vardata: {vardata_type}
"#,
                                    varname = var.name,
                                    enumname = name,
                                    vardata_type = sub_type_name,
                                ),
                            ]
                        }
                    }
                }

                let variant_classes = variants
                    .iter()
                    .flat_map(|var| var_class_def(spec, name, var))
                    .collect::<Vec<_>>()
                    .join("\n");

                let enum_class = {
                    let variant_class_names = variants
                        .iter()
                        .map(|var| format!("{}{}", name, var.name))
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!(
                        r#"{enumname} = typing_extensions.Annotated[typing.Union[{vars}], pydantic.Field(discriminator="var")]"#,
                        enumname = name,
                        vars = variant_class_names
                    )
                };

                format!("{}\n{}\n", variant_classes, enum_class)
            }
        }
    }
}

impl PythonTyper for StructField {
    fn to_python(&self, spec: &ApiSpec) -> String {
        format!(
            "{indent}{}: {}\n",
            self.name,
            self.data.to_python(spec),
            indent = INDENT
        )
    }
}

impl PythonTyper for EnumStructField {
    fn to_python(&self, spec: &ApiSpec) -> String {
        format!(
            "{indent}{}: {}\n",
            self.name,
            self.data.to_python(spec),
            indent = INDENT
        )
    }
}

impl PythonTyper for EnumVariant {
    fn to_python(&self, spec: &ApiSpec) -> String {
        if let EnumVariantData::None = self.data {
            format!("{{ var: \"{}\" }}", self.name)
        } else {
            format!(
                "{{ var: \"{}\", vardata: {} }}",
                self.name,
                self.data.to_python(spec)
            )
        }
    }
}

impl PythonTyper for EnumVariantData {
    fn to_python(&self, spec: &ApiSpec) -> String {
        match self {
            Self::None => "null".into(),
            Self::Single(api_type) => format!("{}", api_type.to_python(spec)),
            Self::Struct(fields) => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_python(spec))
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
    fn python_empty() {
        let spec = ApiSpec {
            module: "TestType".into(),
            types: vec![],
            ..Default::default()
        };

        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

"#
        .trim_start();

        compare_strings(expected, to_python(&spec));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_struct_simple() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestStruct(pydantic.BaseModel):
    foo: int
    bar: str
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_struct_simple()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_struct_with_vec() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestStruct(pydantic.BaseModel):
    foo: list[int]
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_struct_with_vec()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_struct_with_option() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestStruct(pydantic.BaseModel):
    foo: typing.Optional[int]
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_struct_with_option()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_enum_simple() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestEnumFoo(pydantic.BaseModel):
    var: typing.Literal["Foo"] = "Foo"

class TestEnumBar(pydantic.BaseModel):
    var: typing.Literal["Bar"] = "Bar"

class TestEnumQux(pydantic.BaseModel):
    var: typing.Literal["Qux"] = "Qux"

TestEnum = typing_extensions.Annotated[typing.Union[TestEnumFoo, TestEnumBar, TestEnumQux], pydantic.Field(discriminator="var")]
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_enum_simple()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_enum_complex() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestEnumFoo(pydantic.BaseModel):
    var: typing.Literal["Foo"] = "Foo"

class TestEnumBar(pydantic.BaseModel):
    var: typing.Literal["Bar"] = "Bar"
    vardata: bool

class TestEnumQuxData(pydantic.BaseModel):
    sub1: int
    sub2: str

class TestEnumQux(pydantic.BaseModel):
    var: typing.Literal["Qux"] = "Qux"
    vardata: TestEnumQuxData

TestEnum = typing_extensions.Annotated[typing.Union[TestEnumFoo, TestEnumBar, TestEnumQux], pydantic.Field(discriminator="var")]
"#.trim_start();

        compare_strings(expected, to_python(&create_spec_enum_complex()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_enum_with_vec() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestEnumBar(pydantic.BaseModel):
    var: typing.Literal["Bar"] = "Bar"
    vardata: list[int]

class TestEnumQuxData(pydantic.BaseModel):
    sub1: list[bool]

class TestEnumQux(pydantic.BaseModel):
    var: typing.Literal["Qux"] = "Qux"
    vardata: TestEnumQuxData

TestEnum = typing_extensions.Annotated[typing.Union[TestEnumBar, TestEnumQux], pydantic.Field(discriminator="var")]
"#.trim_start();

        compare_strings(expected, to_python(&create_spec_enum_with_vec()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_enum_with_option() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestEnumBar(pydantic.BaseModel):
    var: typing.Literal["Bar"] = "Bar"
    vardata: typing.Optional[int]

class TestEnumQuxData(pydantic.BaseModel):
    sub1: typing.Optional[bool]

class TestEnumQux(pydantic.BaseModel):
    var: typing.Literal["Qux"] = "Qux"
    vardata: TestEnumQuxData

TestEnum = typing_extensions.Annotated[typing.Union[TestEnumBar, TestEnumQux], pydantic.Field(discriminator="var")]
"#.trim_start();

        compare_strings(expected, to_python(&create_spec_enum_with_option()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_nested_option() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestStruct(pydantic.BaseModel):
    x: typing.Optional[typing.Optional[int]]
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_nested_option()));
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
            ..Default::default()
        }
    }

    #[test]
    fn python_nested_array() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestStruct(pydantic.BaseModel):
    x: list[list[int]]
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_nested_array()));
    }

    fn create_spec_map() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "x".into(),
                    data: ApiType::Complex(ComplexApiType::Map(
                        BasicApiType::String,
                        Box::new(ApiType::Basic(BasicApiType::Int)),
                    )),
                }],
            }],
            ..Default::default()
        }
    }

    #[test]
    fn python_map() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestStruct(pydantic.BaseModel):
    x: dict[str, int]
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_map()));
    }

    fn create_spec_lang_specific() -> ApiSpec {
        let rs_type = vec![("py".to_string(), "Tuple[str, int]".to_string())]
            .into_iter()
            .collect();

        let define_custom = vec![("NameAgeTuple".to_string(), rs_type)]
            .into_iter()
            .collect();

        ApiSpec {
            module: "TestType".into(),
            define_custom,
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "x".into(),
                    data: ApiType::Basic(BasicApiType::Custom("NameAgeTuple".into())),
                }],
            }],
        }
    }

    #[test]
    fn python_lang_specific() {
        let expected = r#"
from __future__ import annotations
import typing
import typing_extensions
import pydantic

class TestStruct(pydantic.BaseModel):
    x: Tuple[str, int]
"#
        .trim_start();

        compare_strings(expected, to_python(&create_spec_lang_specific()));
    }
}
