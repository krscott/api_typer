use crate::spec::*;

const LANG: &str = "ts";

pub fn to_typescript(spec: &ApiSpec) -> String {
    spec.to_typescript(spec)
}

pub trait TypescriptTyper {
    fn to_typescript(&self, spec: &ApiSpec) -> String;
}

impl TypescriptTyper for BasicApiType {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        match self {
            BasicApiType::Custom(s) => spec.get_custom_type(s, LANG),
            BasicApiType::Recursive(s) => s.clone(),
            BasicApiType::String => String::from("string"),
            BasicApiType::Int => String::from("number"),
            BasicApiType::Uint => String::from("number"),
            BasicApiType::Float => String::from("number"),
            BasicApiType::Double => String::from("number"),
            BasicApiType::Bool => String::from("boolean"),
        }
    }
}

impl TypescriptTyper for ApiType {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        fn use_parens(inner_type: &ApiType) -> bool {
            match inner_type {
                ApiType::Basic(_) => false,
                ApiType::Complex(ComplexApiType::Option(_)) => true,
                ApiType::Complex(ComplexApiType::Array(_)) => false,
                ApiType::Complex(ComplexApiType::Map(_, _)) => false,
            }
        }

        match self {
            ApiType::Basic(basic_type) => basic_type.to_typescript(spec),
            ApiType::Complex(ComplexApiType::Option(inner_type)) => {
                //TODO: Is there a better way to represent nested `Option`s in Typescript?
                if use_parens(inner_type) {
                    format!("({}) | null", inner_type.to_typescript(spec))
                } else {
                    format!("{} | null", inner_type.to_typescript(spec))
                }
            }
            ApiType::Complex(ComplexApiType::Array(inner_type)) => {
                format!("Array<{}>", inner_type.to_typescript(spec))
            }
            ApiType::Complex(ComplexApiType::Map(key_type, value_type)) => {
                format!(
                    "{{[key: {}]: {}}}",
                    key_type.to_typescript(spec),
                    value_type.to_typescript(spec)
                )
            }
        }
    }
}

impl TypescriptTyper for ApiSpec {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        self.types
            .iter()
            .map(|t| t.to_typescript(spec))
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

impl TypescriptTyper for TypeSpec {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        match self {
            Self::Struct { name, fields } => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_typescript(spec))
                    .collect::<Vec<_>>()
                    .join("");

                format!(
                    "export interface {name} {{\n\
                    {fields}}}",
                    name = name,
                    fields = fields_fmt
                )
            }
            Self::Enum { name, variants } => {
                fn vardata_arg(spec: &ApiSpec, data: &EnumVariantData) -> String {
                    match data {
                        EnumVariantData::None => String::from(""),
                        EnumVariantData::Single(api_type) => {
                            format!("vardata: {}", api_type.to_typescript(spec))
                        }
                        EnumVariantData::Struct(fields) => {
                            let fields_ts = fields
                                .iter()
                                .map(|field| field.to_typescript(spec))
                                .collect::<Vec<_>>()
                                .join("");

                            format!("vardata: {{\n{}{indent}}}", fields_ts, indent = INDENT)
                        }
                    }
                }

                let sep = format!(" |\n{indent}", indent = INDENT);

                let variants_fmt = variants
                    .iter()
                    .map(|var| var.to_typescript(spec))
                    .collect::<Vec<_>>()
                    .join(&sep);

                let variant_objs = variants
                    .iter()
                    .map(|var| {
                        let arg = vardata_arg(spec, &var.data);
                        let comma_vardata = if arg.is_empty() { "" } else { ", vardata" };

                        format!(
                            r#"export const {enumname}{varname}Var = "{varname}"
export const {enumname}{varname}: ({vardata_arg}) => {enumname} = ({vardata_arg}) => ({{ "var": "{varname}"{comma_vardata} }})"#,
                            varname = var.name,
                            enumname = name,
                            comma_vardata = comma_vardata,
                            vardata_arg = arg
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n\n");

                let match_func = {
                    let match_arms = variants
                        .iter()
                        .map(|EnumVariant { name, data }| {
                            let vardata_arg = vardata_arg(spec, data);

                            format!(
                                "{indent}{}: ({}) => T,\n",
                                name,
                                vardata_arg,
                                indent = INDENT,
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("");

                    let match_cases = variants
                        .iter()
                        .map(|EnumVariant { name, data }| {
                            let x_vardata = match data {
                                EnumVariantData::None => "",
                                _ => "x.vardata",
                            };

                            format!(
                                "{indent}{indent}case \"{name}\": return arms.{name}({x_vardata})\n",
                                name = name,
                                x_vardata = x_vardata, indent = INDENT
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("");

                    format!(
                        "export function match{name}<T>(x: {name}, arms: {{\n\
                            {match_arms}\
                        }}): T {{\n\
                            {indent}switch (x.var) {{\n\
                                {match_cases}\
                            {indent}}}\n\
                        }}",
                        name = name,
                        match_arms = match_arms,
                        match_cases = match_cases,
                        indent = INDENT,
                    )
                };

                format!(
                    "export type {name} = (\n\
                    {indent}{variants}\n\
                    )\n\
                    \n\
                    {objects}\n\
                    \n\
                    {match_func}\n",
                    name = name,
                    variants = variants_fmt,
                    objects = variant_objs,
                    match_func = match_func,
                    indent = INDENT
                )
            }
        }
    }
}

impl TypescriptTyper for StructField {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        format!(
            "{indent}{}: {},\n",
            self.name,
            self.data.to_typescript(spec),
            indent = INDENT
        )
    }
}

impl TypescriptTyper for EnumStructField {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        format!(
            "{indent}{indent}{}: {},\n",
            self.name,
            self.data.to_typescript(spec),
            indent = INDENT
        )
    }
}

impl TypescriptTyper for EnumVariant {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        if let EnumVariantData::None = self.data {
            format!("{{ var: \"{}\" }}", self.name)
        } else {
            format!(
                "{{ var: \"{}\", vardata: {} }}",
                self.name,
                self.data.to_typescript(spec)
            )
        }
    }
}

impl TypescriptTyper for EnumVariantData {
    fn to_typescript(&self, spec: &ApiSpec) -> String {
        match self {
            Self::None => "null".into(),
            Self::Single(api_type) => format!("{}", api_type.to_typescript(spec)),
            Self::Struct(fields) => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_typescript(spec))
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
    fn typescript_empty() {
        let spec = ApiSpec {
            module: "TestType".into(),
            types: vec![],
            ..Default::default()
        };

        compare_strings("", to_typescript(&spec));
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
    fn typescript_struct_simple() {
        let expected = r#"export interface TestStruct {
    foo: number,
    bar: string,
}"#;

        compare_strings(expected, to_typescript(&create_spec_struct_simple()));
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
    fn typescript_struct_with_vec() {
        let expected = r#"export interface TestStruct {
    foo: Array<number>,
}"#;

        compare_strings(expected, to_typescript(&create_spec_struct_with_vec()));
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
    fn typescript_struct_with_option() {
        let expected = r#"export interface TestStruct {
    foo: number | null,
}"#;

        compare_strings(expected, to_typescript(&create_spec_struct_with_option()));
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
    fn typescript_enum_single() {
        let expected = r#"export type TestEnum = (
    { var: "Foo" }
)

export const TestEnumFooVar = "Foo"
export const TestEnumFoo: () => TestEnum = () => ({ "var": "Foo" })

export function matchTestEnum<T>(x: TestEnum, arms: {
    Foo: () => T,
}): T {
    switch (x.var) {
        case "Foo": return arms.Foo()
    }
}
"#;

        compare_strings(expected, to_typescript(&create_spec_enum_single()));
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
    fn typescript_enum_simple() {
        let expected = r#"export type TestEnum = (
    { var: "Foo" } |
    { var: "Bar" } |
    { var: "Qux" }
)

export const TestEnumFooVar = "Foo"
export const TestEnumFoo: () => TestEnum = () => ({ "var": "Foo" })

export const TestEnumBarVar = "Bar"
export const TestEnumBar: () => TestEnum = () => ({ "var": "Bar" })

export const TestEnumQuxVar = "Qux"
export const TestEnumQux: () => TestEnum = () => ({ "var": "Qux" })

export function matchTestEnum<T>(x: TestEnum, arms: {
    Foo: () => T,
    Bar: () => T,
    Qux: () => T,
}): T {
    switch (x.var) {
        case "Foo": return arms.Foo()
        case "Bar": return arms.Bar()
        case "Qux": return arms.Qux()
    }
}
"#;

        compare_strings(expected, to_typescript(&create_spec_enum_simple()));
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
    fn typescript_enum_complex() {
        let expected = r#"export type TestEnum = (
    { var: "Foo" } |
    { var: "Bar", vardata: boolean } |
    { var: "Qux", vardata:  {
        sub1: number,
        sub2: string,
    } }
)

export const TestEnumFooVar = "Foo"
export const TestEnumFoo: () => TestEnum = () => ({ "var": "Foo" })

export const TestEnumBarVar = "Bar"
export const TestEnumBar: (vardata: boolean) => TestEnum = (vardata: boolean) => ({ "var": "Bar", vardata })

export const TestEnumQuxVar = "Qux"
export const TestEnumQux: (vardata: {
        sub1: number,
        sub2: string,
    }) => TestEnum = (vardata: {
        sub1: number,
        sub2: string,
    }) => ({ "var": "Qux", vardata })

export function matchTestEnum<T>(x: TestEnum, arms: {
    Foo: () => T,
    Bar: (vardata: boolean) => T,
    Qux: (vardata: {
        sub1: number,
        sub2: string,
    }) => T,
}): T {
    switch (x.var) {
        case "Foo": return arms.Foo()
        case "Bar": return arms.Bar(x.vardata)
        case "Qux": return arms.Qux(x.vardata)
    }
}
"#;

        compare_strings(expected, to_typescript(&create_spec_enum_complex()));
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
    fn typescript_enum_with_vec() {
        let expected = r#"export type TestEnum = (
    { var: "Bar", vardata: Array<number> } |
    { var: "Qux", vardata:  {
        sub1: Array<boolean>,
    } }
)

export const TestEnumBarVar = "Bar"
export const TestEnumBar: (vardata: Array<number>) => TestEnum = (vardata: Array<number>) => ({ "var": "Bar", vardata })

export const TestEnumQuxVar = "Qux"
export const TestEnumQux: (vardata: {
        sub1: Array<boolean>,
    }) => TestEnum = (vardata: {
        sub1: Array<boolean>,
    }) => ({ "var": "Qux", vardata })

export function matchTestEnum<T>(x: TestEnum, arms: {
    Bar: (vardata: Array<number>) => T,
    Qux: (vardata: {
        sub1: Array<boolean>,
    }) => T,
}): T {
    switch (x.var) {
        case "Bar": return arms.Bar(x.vardata)
        case "Qux": return arms.Qux(x.vardata)
    }
}
"#;

        compare_strings(expected, to_typescript(&create_spec_enum_with_vec()));
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
    fn typescript_enum_with_option() {
        let expected = r#"export type TestEnum = (
    { var: "Bar", vardata: number | null } |
    { var: "Qux", vardata:  {
        sub1: boolean | null,
    } }
)

export const TestEnumBarVar = "Bar"
export const TestEnumBar: (vardata: number | null) => TestEnum = (vardata: number | null) => ({ "var": "Bar", vardata })

export const TestEnumQuxVar = "Qux"
export const TestEnumQux: (vardata: {
        sub1: boolean | null,
    }) => TestEnum = (vardata: {
        sub1: boolean | null,
    }) => ({ "var": "Qux", vardata })

export function matchTestEnum<T>(x: TestEnum, arms: {
    Bar: (vardata: number | null) => T,
    Qux: (vardata: {
        sub1: boolean | null,
    }) => T,
}): T {
    switch (x.var) {
        case "Bar": return arms.Bar(x.vardata)
        case "Qux": return arms.Qux(x.vardata)
    }
}
"#;

        compare_strings(expected, to_typescript(&create_spec_enum_with_option()));
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
    fn typescript_nested_option() {
        let expected = r#"
export interface TestStruct {
    x: (number | null) | null,
}
"#
        .trim();

        compare_strings(expected, to_typescript(&create_spec_nested_option()));
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
    fn typescript_nested_array() {
        let expected = r#"
export interface TestStruct {
    x: Array<Array<number>>,
}
"#
        .trim();

        compare_strings(expected, to_typescript(&create_spec_nested_array()));
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
    fn typescript_map() {
        let expected = r#"
export interface TestStruct {
    x: {[key: string]: number},
}
"#
        .trim();

        compare_strings(expected, to_typescript(&create_spec_map()));
    }

    fn create_spec_lang_specific() -> ApiSpec {
        let rs_type = vec![("ts".to_string(), "{ locked: bool, bit: bool }".to_string())]
            .into_iter()
            .collect();

        let define_custom = vec![("ThreadBit".to_string(), rs_type)]
            .into_iter()
            .collect();

        ApiSpec {
            module: "TestType".into(),
            define_custom,
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "x".into(),
                    data: ApiType::Basic(BasicApiType::Custom("ThreadBit".into())),
                }],
            }],
        }
    }

    #[test]
    fn typescript_lang_specific() {
        let expected = r#"
export interface TestStruct {
    x: { locked: bool, bit: bool },
}
"#
        .trim();

        compare_strings(expected, to_typescript(&create_spec_lang_specific()));
    }
}
