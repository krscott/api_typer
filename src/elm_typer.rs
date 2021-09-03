use crate::spec::*;

pub fn to_elm(spec: &ApiSpec) -> String {
    spec.to_elm()
}

pub trait ElmTyper {
    fn to_elm(&self) -> String;
    fn to_elm_decoder(&self) -> String {
        unimplemented!()
    }
    fn to_elm_encoder(&self) -> String {
        unimplemented!()
    }
}

impl ElmTyper for BasicApiType {
    fn to_elm(&self) -> String {
        match self {
            BasicApiType::Custom(s) => s.clone(),
            BasicApiType::Recursive(s) => s.clone(),
            BasicApiType::String => String::from("String"),
            BasicApiType::Int => String::from("Int"),
            BasicApiType::Uint => String::from("Int"),
            BasicApiType::Float => String::from("Float"),
            BasicApiType::Double => String::from("Float"),
            BasicApiType::Bool => String::from("Bool"),
        }
    }
}

impl ElmTyper for ApiType {
    fn to_elm(&self) -> String {
        fn use_parens(inner_type: &ApiType) -> bool {
            match inner_type {
                ApiType::Basic(_) => false,
                ApiType::Complex(_) => true,
            }
        }

        match self {
            ApiType::Basic(basic_type) => basic_type.to_elm(),
            ApiType::Complex(ComplexApiType::Option(inner_type)) => {
                if use_parens(inner_type) {
                    format!("Maybe ({})", inner_type.to_elm())
                } else {
                    format!("Maybe {}", inner_type.to_elm())
                }
            }
            ApiType::Complex(ComplexApiType::Array(inner_type)) => {
                if use_parens(inner_type) {
                    format!("List ({})", inner_type.to_elm())
                } else {
                    format!("List {}", inner_type.to_elm())
                }
            }
            ApiType::Complex(ComplexApiType::Map(key_type @ BasicApiType::String, value_type)) => {
                if use_parens(value_type) {
                    format!("Dict {} ({})", key_type.to_elm(), value_type.to_elm())
                } else {
                    format!("Dict {} {}", key_type.to_elm(), value_type.to_elm())
                }
            }
            ApiType::Complex(ComplexApiType::Map(key_type, _)) => {
                unimplemented!("Dict key type '{}' not supported", key_type.to_elm())
            }
        }
    }
}

impl ElmTyper for ApiSpec {
    fn to_elm(&self) -> String {
        let exports_str = self
            .types
            .iter()
            .flat_map(|t| {
                let (name, expose) = match t {
                    TypeSpec::Struct { name, .. } => (name, name.clone()),
                    TypeSpec::Enum { name, .. } => (name, format!("{}(..)", name)),
                };
                vec![
                    expose.clone(),
                    format!("decode{}", name),
                    format!("encode{}", name),
                ]
            })
            .collect::<Vec<_>>()
            .join(", ");

        let types_str = self
            .types
            .iter()
            .flat_map(|t| vec![t.to_elm(), t.to_elm_decoder(), t.to_elm_encoder()])
            .collect::<Vec<_>>()
            .join("\n\n");

        let dict_import = if types_str.contains("Dict ") {
            "import Dict exposing (Dict)\n"
        } else {
            ""
        };

        format!(
            "\
            module {name} exposing ({exports})\n\
            \n\
            import Json.Decode\n\
            import Json.Decode.Extra\n\
            import Json.Decode.Pipeline\n\
            import Json.Encode\n\
            import Json.Encode.Extra\n\
            {dict_import}\
            \n\
            {types}",
            name = self.module,
            exports = exports_str,
            dict_import = dict_import,
            types = types_str
        )
    }
}

impl ElmTyper for TypeSpec {
    fn to_elm(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let sep = format!("\n{indent}, ", indent = INDENT);

                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_elm())
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    type alias {name} =\n\
                    {indent}{{ {fields}\n\
                    {indent}}}",
                    name = name,
                    fields = fields_fmt,
                    indent = INDENT,
                )
            }
            Self::Enum { name, variants } => {
                let subtypes = variants
                    .iter()
                    .filter_map(|var| {
                        if let EnumVariantData::Struct(fields) = &var.data {
                            let subtype = TypeSpec::Struct {
                                name: format!("{}{}", name, var.name),
                                fields: fields.iter().map(|field| field.clone().into()).collect(),
                            };
                            Some(format!(
                                "{}\n\n{}\n\n",
                                subtype.to_elm(),
                                subtype.to_elm_decoder()
                            ))
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("");

                let sep = format!("\n{indent}| ", indent = INDENT);

                let variants_fmt = variants
                    .iter()
                    .map(|var| {
                        if let EnumVariantData::Struct(_) = &var.data {
                            format!("{name} {parent}{name}", name = var.name, parent = name)
                        } else {
                            var.to_elm()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    {subtypes}\
                    type {name}\n\
                    {indent}= {variants}",
                    subtypes = subtypes,
                    name = name,
                    variants = variants_fmt,
                    indent = INDENT,
                )
            }
        }
    }

    fn to_elm_decoder(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let sep = format!("\n{indent}{indent}", indent = INDENT);

                let field_decoders = fields
                    .iter()
                    .map(|field| format!("|> {}", field.to_elm_decoder()))
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    decode{name} : Json.Decode.Decoder {name}\n\
                    decode{name} =\n\
                    {indent}Json.Decode.succeed {name}\n\
                    {indent}{indent}{fields}",
                    name = name,
                    fields = field_decoders,
                    indent = INDENT
                )
            }
            Self::Enum { name, variants } => {
                let sep = format!("\n{indent}{indent}, ", indent = INDENT);

                let variant_decoders = variants
                    .iter()
                    .map(|var| if let EnumVariantData::Struct(_) = &var.data {
                        format!(
                            "Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"{name}\") <|\n\
                            {indent}{indent}{indent}Json.Decode.map {name} (Json.Decode.field \"vardata\" <| decode{parent}{name})",
                            name = var.name,
                            parent = name, indent = INDENT,
                        )
                    } else {
                        var.to_elm_decoder()
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    decode{name} : Json.Decode.Decoder {name}\n\
                    decode{name} =\n\
                    {indent}Json.Decode.oneOf\n\
                    {indent}{indent}[ {variants}\n\
                    {indent}{indent}]",
                    name = name,
                    variants = variant_decoders,
                    indent = INDENT
                )
            }
        }
    }

    fn to_elm_encoder(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let sep = format!("\n{indent}{indent}, ", indent = INDENT);

                let field_encoders = fields
                    .iter()
                    .map(|field| field.to_elm_encoder())
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    encode{name} : {name} -> Json.Encode.Value\n\
                    encode{name} record =\n\
                    {indent}Json.Encode.object\n\
                    {indent}{indent}[ {fields}\n\
                    {indent}{indent}]",
                    name = name,
                    fields = field_encoders,
                    indent = INDENT
                )
            }
            Self::Enum { name, variants } => {
                let variant_cases = variants
                    .iter()
                    .map(|var| var.to_elm_encoder())
                    .collect::<Vec<_>>()
                    .join("");

                format!(
                    "\
                    encode{name} : {name} -> Json.Encode.Value\n\
                    encode{name} var =\n\
                    {indent}case var of{variants}",
                    name = name,
                    variants = variant_cases,
                    indent = INDENT
                )
            }
        }
    }
}

impl ElmTyper for StructField {
    fn to_elm(&self) -> String {
        let elm_type = self.data.to_elm();

        if elm_type.contains(' ') {
            format!("{} : ({})", self.name, elm_type)
        } else {
            format!("{} : {}", self.name, elm_type)
        }
    }

    fn to_elm_decoder(&self) -> String {
        format!(
            "Json.Decode.Pipeline.required \"{name}\" {decoder}",
            name = self.name,
            decoder = elm_json_decoder(&self.data)
        )
    }

    fn to_elm_encoder(&self) -> String {
        format!(
            "(\"{name}\", {encoder} <| record.{name})",
            name = self.name,
            encoder = elm_json_encoder(&self.data)
        )
    }
}

impl ElmTyper for EnumVariant {
    fn to_elm(&self) -> String {
        match &self.data {
            EnumVariantData::None => format!("{}", self.name),
            EnumVariantData::Single(api_type) => {
                let elm_type = api_type.to_elm();
                if elm_type.contains(' ') {
                    format!("{} ({})", self.name, elm_type)
                } else {
                    format!("{} {}", self.name, elm_type)
                }
            }
            EnumVariantData::Struct(_fields) => {
                // Caller is responsible for adding parent:
                //   format!("{name} {parent}{name}", ...)
                panic!("Cannot derive Elm EnumVariant::Struct")
            }
        }
    }

    fn to_elm_decoder(&self) -> String {
        match &self.data {
            EnumVariantData::None => format!(
                "Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"{name}\") <|\n\
                {indent}{indent}{indent}Json.Decode.succeed {name}",
                name = self.name, indent = INDENT,
            ),
            EnumVariantData::Single(api_type) => {
                format!(
                    "Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"{name}\") <|\n\
                    {indent}{indent}{indent}Json.Decode.map {name} (Json.Decode.field \"vardata\" <| {decoder})",
                    name = self.name,
                    decoder = elm_json_decoder(api_type), indent = INDENT,
                )
            },
            EnumVariantData::Struct(_) => {
                panic!("Cannot derive Elm EnumVariant::Struct")
            }
        }
    }

    fn to_elm_encoder(&self) -> String {
        match &self.data {
            EnumVariantData::None => format!(
                "\n\
                {indent}{indent}{name} ->\n\
                {indent}{indent}{indent}Json.Encode.object\n\
                {indent}{indent}{indent}{indent}[ ( \"var\", Json.Encode.string \"{name}\" )\n\
                {indent}{indent}{indent}{indent}]",
                name = self.name,
                indent = INDENT
            ),
            EnumVariantData::Single(api_type) => {
                format!(
                    "\n\
                    {indent}{indent}{name} value ->\n\
                    {indent}{indent}{indent}Json.Encode.object\n\
                    {indent}{indent}{indent}{indent}[ ( \"var\", Json.Encode.string \"{name}\" )\n\
                    {indent}{indent}{indent}{indent}, ( \"vardata\", {encoder} <| value )\n\
                    {indent}{indent}{indent}{indent}]",
                    name = self.name,
                    encoder = elm_json_encoder(api_type),
                    indent = INDENT
                )
            }
            EnumVariantData::Struct(fields) => {
                let sep = format!(
                    "\n{indent}{indent}{indent}{indent}{indent},",
                    indent = INDENT
                );

                let encoder = fields
                    .iter()
                    .map(|field| {
                        format!(
                            " ( \"{name}\", {encoder} <| record.{name} )",
                            name = field.name,
                            encoder = elm_json_encoder(&field.data)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\n\
                {indent}{indent}{name} record ->\n\
                {indent}{indent}{indent}Json.Encode.object\n\
                {indent}{indent}{indent}{indent}[ ( \"var\", Json.Encode.string \"{name}\" )\n\
                {indent}{indent}{indent}{indent}, ( \"vardata\", Json.Encode.object\n\
                {indent}{indent}{indent}{indent}{indent}[{encoder}\n\
                {indent}{indent}{indent}{indent}{indent}] )\n\
                {indent}{indent}{indent}{indent}]",
                    name = self.name,
                    encoder = encoder,
                    indent = INDENT,
                )
            }
        }
    }
}

fn elm_json_decoder(data: &ApiType) -> String {
    match data {
        ApiType::Basic(t) => {
            const JSON_ENCODE_TYPES: &[&str] = &["String", "Int", "Float", "Bool"];

            let t = t.to_elm();

            if JSON_ENCODE_TYPES.contains(&t.as_str()) {
                format!("Json.Decode.{}", t.to_lowercase())
            } else {
                format!("decode{}", t)
            }
        }
        ApiType::Complex(ComplexApiType::Option(t)) => {
            format!("(Json.Decode.nullable {})", elm_json_decoder(t))
        }
        ApiType::Complex(ComplexApiType::Array(t)) => {
            format!("(Json.Decode.list {})", elm_json_decoder(t))
        }
        ApiType::Complex(ComplexApiType::Map(BasicApiType::String, value_type)) => {
            format!("(Json.Decode.dict {})", value_type.to_elm())
        }
        ApiType::Complex(ComplexApiType::Map(key_type, _)) => {
            unimplemented!("Dict key type '{}' not supported", key_type.to_elm())
        }
    }
}

fn elm_json_encoder(data: &ApiType) -> String {
    match data {
        ApiType::Basic(t) => {
            const JSON_ENCODE_TYPES: &[&str] = &["String", "Int", "Float", "Bool"];

            let t = t.to_elm();

            if JSON_ENCODE_TYPES.contains(&t.as_str()) {
                format!("Json.Encode.{}", t.to_lowercase())
            } else {
                format!("encode{}", t)
            }
        }
        ApiType::Complex(ComplexApiType::Option(t)) => {
            format!("(Json.Encode.Extra.maybe {})", elm_json_encoder(t))
        }
        ApiType::Complex(ComplexApiType::Array(t)) => {
            format!("(Json.Encode.list {})", elm_json_encoder(t))
        }
        ApiType::Complex(ComplexApiType::Map(BasicApiType::String, value_type)) => {
            format!("(Json.Encode.dict identity {})", value_type.to_elm())
        }
        ApiType::Complex(ComplexApiType::Map(key_type, _)) => {
            unimplemented!("Dict key type '{}' not supported", key_type.to_elm())
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
    fn elm_empty() {
        let spec = ApiSpec {
            module: "TestType".into(),
            types: vec![],
        };

        compare_strings(
            "\
module TestType exposing ()

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

",
            spec.to_elm(),
        );
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
    fn elm_struct_simple() {
        let expected = r#"
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
    { foo : Int
    , bar : String
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required "foo" Json.Decode.int
        |> Json.Decode.Pipeline.required "bar" Json.Decode.string

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ ("foo", Json.Encode.int <| record.foo)
        , ("bar", Json.Encode.string <| record.bar)
        ]
"#
        .trim();

        compare_strings(expected, create_spec_struct_simple().to_elm());
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
    fn elm_struct_with_vec() {
        let expected = r#"
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
    { foo : (List Int)
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required "foo" (Json.Decode.list Json.Decode.int)

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ ("foo", (Json.Encode.list Json.Encode.int) <| record.foo)
        ]
"#
        .trim();

        compare_strings(expected, create_spec_struct_with_vec().to_elm());
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
    fn elm_struct_with_option() {
        let expected = r#"
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
    { foo : (Maybe Int)
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required "foo" (Json.Decode.nullable Json.Decode.int)

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ ("foo", (Json.Encode.Extra.maybe Json.Encode.int) <| record.foo)
        ]
"#
        .trim();

        compare_strings(expected, create_spec_struct_with_option().to_elm());
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
    fn elm_enum_simple() {
        let expected = r#"
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type TestEnum
    = Foo
    | Bar
    | Qux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Foo") <|
            Json.Decode.succeed Foo
        , Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Bar") <|
            Json.Decode.succeed Bar
        , Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Qux") <|
            Json.Decode.succeed Qux
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Foo ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Foo" )
                ]
        Bar ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Bar" )
                ]
        Qux ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Qux" )
                ]
"#
        .trim();

        compare_strings(expected, create_spec_enum_simple().to_elm());
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
    fn elm_enum_complex() {
        let expected = r#"
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestEnumQux =
    { sub1 : Int
    , sub2 : String
    }

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
    Json.Decode.succeed TestEnumQux
        |> Json.Decode.Pipeline.required "sub1" Json.Decode.int
        |> Json.Decode.Pipeline.required "sub2" Json.Decode.string

type TestEnum
    = Foo
    | Bar Bool
    | Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Foo") <|
            Json.Decode.succeed Foo
        , Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Bar") <|
            Json.Decode.map Bar (Json.Decode.field "vardata" <| Json.Decode.bool)
        , Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Qux") <|
            Json.Decode.map Qux (Json.Decode.field "vardata" <| decodeTestEnumQux)
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Foo ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Foo" )
                ]
        Bar value ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Bar" )
                , ( "vardata", Json.Encode.bool <| value )
                ]
        Qux record ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Qux" )
                , ( "vardata", Json.Encode.object
                    [ ( "sub1", Json.Encode.int <| record.sub1 )
                    , ( "sub2", Json.Encode.string <| record.sub2 )
                    ] )
                ]
"#
        .trim();

        compare_strings(expected, create_spec_enum_complex().to_elm());
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
    fn elm_enum_with_vec() {
        let expected = r#"
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestEnumQux =
    { sub1 : (List Bool)
    }

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
    Json.Decode.succeed TestEnumQux
        |> Json.Decode.Pipeline.required "sub1" (Json.Decode.list Json.Decode.bool)

type TestEnum
    = Bar (List Int)
    | Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Bar") <|
            Json.Decode.map Bar (Json.Decode.field "vardata" <| (Json.Decode.list Json.Decode.int))
        , Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Qux") <|
            Json.Decode.map Qux (Json.Decode.field "vardata" <| decodeTestEnumQux)
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Bar value ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Bar" )
                , ( "vardata", (Json.Encode.list Json.Encode.int) <| value )
                ]
        Qux record ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Qux" )
                , ( "vardata", Json.Encode.object
                    [ ( "sub1", (Json.Encode.list Json.Encode.bool) <| record.sub1 )
                    ] )
                ]
"#
        .trim();

        compare_strings(expected, create_spec_enum_with_vec().to_elm());
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
    fn elm_enum_with_option() {
        let expected = r#"
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestEnumQux =
    { sub1 : (Maybe Bool)
    }

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
    Json.Decode.succeed TestEnumQux
        |> Json.Decode.Pipeline.required "sub1" (Json.Decode.nullable Json.Decode.bool)

type TestEnum
    = Bar (Maybe Int)
    | Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Bar") <|
            Json.Decode.map Bar (Json.Decode.field "vardata" <| (Json.Decode.nullable Json.Decode.int))
        , Json.Decode.Extra.when (Json.Decode.field "var" Json.Decode.string) ((==) "Qux") <|
            Json.Decode.map Qux (Json.Decode.field "vardata" <| decodeTestEnumQux)
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Bar value ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Bar" )
                , ( "vardata", (Json.Encode.Extra.maybe Json.Encode.int) <| value )
                ]
        Qux record ->
            Json.Encode.object
                [ ( "var", Json.Encode.string "Qux" )
                , ( "vardata", Json.Encode.object
                    [ ( "sub1", (Json.Encode.Extra.maybe Json.Encode.bool) <| record.sub1 )
                    ] )
                ]
"#.trim();

        compare_strings(expected, create_spec_enum_with_option().to_elm());
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
    fn elm_nested_option() {
        let expected = r#"
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
    { x : (Maybe (Maybe Int))
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required "x" (Json.Decode.nullable (Json.Decode.nullable Json.Decode.int))

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ ("x", (Json.Encode.Extra.maybe (Json.Encode.Extra.maybe Json.Encode.int)) <| record.x)
        ]
"#.trim();

        compare_strings(expected, create_spec_nested_option().to_elm());
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
    fn elm_nested_array() {
        let expected = r#"
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
    { x : (List (List Int))
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required "x" (Json.Decode.list (Json.Decode.list Json.Decode.int))

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ ("x", (Json.Encode.list (Json.Encode.list Json.Encode.int)) <| record.x)
        ]
"#
        .trim();

        compare_strings(expected, create_spec_nested_array().to_elm());
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
        }
    }

    #[test]
    fn elm_map() {
        let expected = r#"
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra
import Dict exposing (Dict)

type alias TestStruct =
    { x : (Dict String Int)
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required "x" (Json.Decode.dict Int)

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ ("x", (Json.Encode.dict identity Int) <| record.x)
        ]
"#
        .trim();

        compare_strings(expected, create_spec_map().to_elm());
    }
}
