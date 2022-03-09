use crate::spec::*;

const LANG: &str = "py";

const HEADER: &str = r#"
from __future__ import annotations
import inspect
import pydantic
import typing
import typing_extensions

class EnumBaseModel(pydantic.BaseModel):
    def __eq__(self, other: typing.Any):
        self_root = getattr(self, "__root__", None)
        other = getattr(other, "__root__", other)
        if self_root is None:
            return super().__eq__(other)
        return self_root == other
"#;

const FOOTER: &str = r#"
for x in list(locals().values()):
    if inspect.isclass(x) and issubclass(x, pydantic.BaseModel):
        x.update_forward_refs()
"#;

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

        format!("{}\n\n{}\n\n{}", HEADER.trim(), types.trim(), FOOTER.trim())
    }
}

fn enum_var_class_name(name: &str, var: &EnumVariant) -> String {
    format!("{}_{}", name, var.name)
}

fn enum_struct_names_types(spec: &ApiSpec, fields: &[EnumStructField]) -> Vec<(String, String)> {
    fields
        .iter()
        .map(|field| (field.name.clone(), field.data.to_python(spec)))
        .collect()
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
                            r#"class {classname}(EnumBaseModel):
    var: typing.Literal["{varname}"] = "{varname}"
"#,
                            classname = enum_var_class_name(name, var),
                            varname = var.name,
                        )],
                        EnumVariantData::Single(api_type) => vec![format!(
                            r#"class {classname}(EnumBaseModel):
    var: typing.Literal["{varname}"] = "{varname}"
    vardata: {datatype}
"#,
                            classname = enum_var_class_name(name, var),
                            varname = var.name,
                            datatype = api_type.to_python(spec)
                        )],
                        EnumVariantData::Struct(fields) => {
                            let class_name = enum_var_class_name(name, var);
                            let inner_class_name = format!("{}_Data", class_name);

                            let fields_fmt = fields
                                .iter()
                                .map(|field| field.to_python(spec))
                                .collect::<Vec<_>>()
                                .join("");

                            vec![
                                format!(
                                    "class {inner_class_name}(pydantic.BaseModel):\n{fields}",
                                    inner_class_name = inner_class_name,
                                    fields = fields_fmt
                                ),
                                format!(
                                    r#"class {classname}(EnumBaseModel):
    var: typing.Literal["{varname}"] = "{varname}"
    vardata: {vardata_type}
"#,
                                    classname = class_name,
                                    varname = var.name,
                                    vardata_type = inner_class_name,
                                ),
                            ]
                        }
                    }
                }

                fn var_constructor(spec: &ApiSpec, name: &str, var: &EnumVariant) -> String {
                    match &var.data {
                        EnumVariantData::None => format!(
                            r#"
    @classmethod
    def {varname}(cls) -> {name}:
        return cls.parse_obj({{"var": "{varname}"}})
"#,
                            name = name,
                            varname = var.name
                        ),
                        EnumVariantData::Single(api_type) => format!(
                            r#"
    @classmethod
    def {varname}(cls, vardata: {datatype}) -> {name}:
        return cls.parse_obj({{"var": "{varname}", "vardata": vardata}})
"#,
                            name = name,
                            varname = var.name,
                            datatype = api_type.to_python(spec)
                        ),
                        EnumVariantData::Struct(fields) => {
                            let names_types = enum_struct_names_types(spec, fields);

                            let args = names_types
                                .iter()
                                .map(|(name, type_)| format!("{}: {}", name, type_))
                                .collect::<Vec<_>>()
                                .join(", ");

                            let obj = names_types
                                .iter()
                                .map(|(name, _)| format!("\"{name}\": {name}", name = name))
                                .collect::<Vec<_>>()
                                .join(", ");

                            format!(
                                r#"
    @classmethod
    def {varname}(cls, *, {args}) -> {name}:
        return cls.parse_obj({{"var": "{varname}", "vardata": {{{obj}}}}})
"#,
                                name = name,
                                varname = var.name,
                                args = args,
                                obj = obj
                            )
                        }
                    }
                }

                fn var_cast(name: &str, var: &EnumVariant) -> String {
                    format!(
                        r#"
    def as_{varname}(self) -> typing.Optional[{classname}]:
        if isinstance(self.__root__, {classname}):
            return self.__root__
        return None
"#,
                        classname = enum_var_class_name(name, var),
                        varname = var.name
                    )
                }

                fn var_cast_nocheck(name: &str, var: &EnumVariant) -> String {
                    format!(
                        r#"
    def as_{varname}(self) -> typing.Optional[{classname}]:
        return self.__root__
"#,
                        classname = enum_var_class_name(name, var),
                        varname = var.name
                    )
                }

                let variant_class_names = variants
                    .iter()
                    .map(|var| format!("{}_{}", name, var.name))
                    .collect::<Vec<_>>();

                let variant_classes = variants
                    .iter()
                    .flat_map(|var| var_class_def(spec, name, var))
                    .collect::<Vec<_>>()
                    .join("\n");

                let var_constructors = variants
                    .iter()
                    .map(|var| var_constructor(spec, name, var))
                    .collect::<Vec<_>>()
                    .join("");

                let var_casts = if variants.len() == 1 {
                    var_cast_nocheck(name, &variants[0])
                } else {
                    variants
                        .iter()
                        .map(|var| var_cast(name, var))
                        .collect::<Vec<_>>()
                        .join("")
                };

                eprintln!("{:?}", variant_class_names);
                let enum_type = if variant_class_names.len() == 1 {
                    format!(
                        r#"_{enumname}_type = {vars}"#,
                        enumname = name,
                        vars = variant_class_names[0]
                    )
                } else {
                    format!(
                        r#"_{enumname}_type = typing_extensions.Annotated[typing.Union[{vars}], pydantic.Field(discriminator="var")]"#,
                        enumname = name,
                        vars = variant_class_names.join(", ")
                    )
                };

                let enum_class = format!(
                    r#"
class {enumname}(EnumBaseModel):
    __root__: _{enumname}_type
{var_constructors}{var_casts}
                        "#,
                    enumname = name,
                    var_constructors = var_constructors,
                    var_casts = var_casts,
                );

                format!("{}\n{}\n{}\n", variant_classes, enum_type, enum_class)
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

    fn py_with_header(s: &str) -> String {
        format!("{}\n\n{}\n\n{}", HEADER.trim(), s.trim(), FOOTER.trim())
    }

    fn compare_strings(expected: &str, actual: String) {
        eprintln!(
            "============\n  Expected\n============\n\n{}\n\n\
            ==========\n  Actual\n==========\n\n{}\n\n",
            expected, actual
        );
        assert_eq!(expected.trim(), actual.trim());
    }

    #[test]
    fn python_empty() {
        let spec = ApiSpec {
            module: "TestType".into(),
            types: vec![],
            ..Default::default()
        };

        let expected = py_with_header("");

        compare_strings(&expected, to_python(&spec));
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
        let expected = py_with_header(
            r#"
class TestStruct(pydantic.BaseModel):
    foo: int
    bar: str
"#,
        );

        compare_strings(&expected, to_python(&create_spec_struct_simple()));
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
        let expected = py_with_header(
            r#"
class TestStruct(pydantic.BaseModel):
    foo: list[int]
"#,
        );

        compare_strings(&expected, to_python(&create_spec_struct_with_vec()));
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
        let expected = py_with_header(
            r#"
class TestStruct(pydantic.BaseModel):
    foo: typing.Optional[int]
"#,
        );

        compare_strings(&expected, to_python(&create_spec_struct_with_option()));
    }

    fn create_spec_enum_single() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Enum {
                name: "TestEnum".into(),
                variants: vec![EnumVariant {
                    name: "Foo".into(),
                    data: EnumVariantData::None,
                }],
            }],
            ..Default::default()
        }
    }

    #[test]
    fn python_enum_single() {
        let expected = py_with_header(
            r#"
class TestEnum_Foo(EnumBaseModel):
    var: typing.Literal["Foo"] = "Foo"

_TestEnum_type = TestEnum_Foo

class TestEnum(EnumBaseModel):
    __root__: _TestEnum_type

    @classmethod
    def Foo(cls) -> TestEnum:
        return cls.parse_obj({"var": "Foo"})

    def as_Foo(self) -> typing.Optional[TestEnum_Foo]:
        return self.__root__
"#,
        );

        compare_strings(&expected, to_python(&create_spec_enum_single()));
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
        let expected = py_with_header(
            r#"
class TestEnum_Foo(EnumBaseModel):
    var: typing.Literal["Foo"] = "Foo"

class TestEnum_Bar(EnumBaseModel):
    var: typing.Literal["Bar"] = "Bar"

class TestEnum_Qux(EnumBaseModel):
    var: typing.Literal["Qux"] = "Qux"

_TestEnum_type = typing_extensions.Annotated[typing.Union[TestEnum_Foo, TestEnum_Bar, TestEnum_Qux], pydantic.Field(discriminator="var")]

class TestEnum(EnumBaseModel):
    __root__: _TestEnum_type

    @classmethod
    def Foo(cls) -> TestEnum:
        return cls.parse_obj({"var": "Foo"})

    @classmethod
    def Bar(cls) -> TestEnum:
        return cls.parse_obj({"var": "Bar"})

    @classmethod
    def Qux(cls) -> TestEnum:
        return cls.parse_obj({"var": "Qux"})

    def as_Foo(self) -> typing.Optional[TestEnum_Foo]:
        if isinstance(self.__root__, TestEnum_Foo):
            return self.__root__
        return None

    def as_Bar(self) -> typing.Optional[TestEnum_Bar]:
        if isinstance(self.__root__, TestEnum_Bar):
            return self.__root__
        return None

    def as_Qux(self) -> typing.Optional[TestEnum_Qux]:
        if isinstance(self.__root__, TestEnum_Qux):
            return self.__root__
        return None
"#,
        );

        compare_strings(&expected, to_python(&create_spec_enum_simple()));
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
        let expected = py_with_header(
            r#"
class TestEnum_Foo(EnumBaseModel):
    var: typing.Literal["Foo"] = "Foo"

class TestEnum_Bar(EnumBaseModel):
    var: typing.Literal["Bar"] = "Bar"
    vardata: bool

class TestEnum_Qux_Data(pydantic.BaseModel):
    sub1: int
    sub2: str

class TestEnum_Qux(EnumBaseModel):
    var: typing.Literal["Qux"] = "Qux"
    vardata: TestEnum_Qux_Data

_TestEnum_type = typing_extensions.Annotated[typing.Union[TestEnum_Foo, TestEnum_Bar, TestEnum_Qux], pydantic.Field(discriminator="var")]

class TestEnum(EnumBaseModel):
    __root__: _TestEnum_type

    @classmethod
    def Foo(cls) -> TestEnum:
        return cls.parse_obj({"var": "Foo"})

    @classmethod
    def Bar(cls, vardata: bool) -> TestEnum:
        return cls.parse_obj({"var": "Bar", "vardata": vardata})

    @classmethod
    def Qux(cls, *, sub1: int, sub2: str) -> TestEnum:
        return cls.parse_obj({"var": "Qux", "vardata": {"sub1": sub1, "sub2": sub2}})

    def as_Foo(self) -> typing.Optional[TestEnum_Foo]:
        if isinstance(self.__root__, TestEnum_Foo):
            return self.__root__
        return None

    def as_Bar(self) -> typing.Optional[TestEnum_Bar]:
        if isinstance(self.__root__, TestEnum_Bar):
            return self.__root__
        return None

    def as_Qux(self) -> typing.Optional[TestEnum_Qux]:
        if isinstance(self.__root__, TestEnum_Qux):
            return self.__root__
        return None
"#,
        );

        compare_strings(&expected, to_python(&create_spec_enum_complex()));
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
        let expected = py_with_header(
            r#"
class TestEnum_Bar(EnumBaseModel):
    var: typing.Literal["Bar"] = "Bar"
    vardata: list[int]

class TestEnum_Qux_Data(pydantic.BaseModel):
    sub1: list[bool]

class TestEnum_Qux(EnumBaseModel):
    var: typing.Literal["Qux"] = "Qux"
    vardata: TestEnum_Qux_Data

_TestEnum_type = typing_extensions.Annotated[typing.Union[TestEnum_Bar, TestEnum_Qux], pydantic.Field(discriminator="var")]

class TestEnum(EnumBaseModel):
    __root__: _TestEnum_type

    @classmethod
    def Bar(cls, vardata: list[int]) -> TestEnum:
        return cls.parse_obj({"var": "Bar", "vardata": vardata})

    @classmethod
    def Qux(cls, *, sub1: list[bool]) -> TestEnum:
        return cls.parse_obj({"var": "Qux", "vardata": {"sub1": sub1}})

    def as_Bar(self) -> typing.Optional[TestEnum_Bar]:
        if isinstance(self.__root__, TestEnum_Bar):
            return self.__root__
        return None

    def as_Qux(self) -> typing.Optional[TestEnum_Qux]:
        if isinstance(self.__root__, TestEnum_Qux):
            return self.__root__
        return None
"#,
        );

        compare_strings(&expected, to_python(&create_spec_enum_with_vec()));
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
        let expected = py_with_header(
            r#"
class TestEnum_Bar(EnumBaseModel):
    var: typing.Literal["Bar"] = "Bar"
    vardata: typing.Optional[int]

class TestEnum_Qux_Data(pydantic.BaseModel):
    sub1: typing.Optional[bool]

class TestEnum_Qux(EnumBaseModel):
    var: typing.Literal["Qux"] = "Qux"
    vardata: TestEnum_Qux_Data

_TestEnum_type = typing_extensions.Annotated[typing.Union[TestEnum_Bar, TestEnum_Qux], pydantic.Field(discriminator="var")]

class TestEnum(EnumBaseModel):
    __root__: _TestEnum_type

    @classmethod
    def Bar(cls, vardata: typing.Optional[int]) -> TestEnum:
        return cls.parse_obj({"var": "Bar", "vardata": vardata})

    @classmethod
    def Qux(cls, *, sub1: typing.Optional[bool]) -> TestEnum:
        return cls.parse_obj({"var": "Qux", "vardata": {"sub1": sub1}})

    def as_Bar(self) -> typing.Optional[TestEnum_Bar]:
        if isinstance(self.__root__, TestEnum_Bar):
            return self.__root__
        return None

    def as_Qux(self) -> typing.Optional[TestEnum_Qux]:
        if isinstance(self.__root__, TestEnum_Qux):
            return self.__root__
        return None
"#,
        );

        compare_strings(&expected, to_python(&create_spec_enum_with_option()));
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
        let expected = py_with_header(
            r#"
class TestStruct(pydantic.BaseModel):
    x: typing.Optional[typing.Optional[int]]
"#,
        );

        compare_strings(&expected, to_python(&create_spec_nested_option()));
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
        let expected = py_with_header(
            r#"
class TestStruct(pydantic.BaseModel):
    x: list[list[int]]
"#,
        );

        compare_strings(&expected, to_python(&create_spec_nested_array()));
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
        let expected = py_with_header(
            r#"
class TestStruct(pydantic.BaseModel):
    x: dict[str, int]
"#,
        );

        compare_strings(&expected, to_python(&create_spec_map()));
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
        let expected = py_with_header(
            r#"
class TestStruct(pydantic.BaseModel):
    x: Tuple[str, int]
"#,
        );

        compare_strings(&expected, to_python(&create_spec_lang_specific()));
    }
}
