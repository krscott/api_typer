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
        match self {
            ApiType::Basic(basic_type) => basic_type.to_elm(),
            ApiType::Complex(ComplexApiType::Option(basic_type)) => {
                format!("Maybe {}", basic_type.to_elm())
            }
            ApiType::Complex(ComplexApiType::Array(basic_type)) => {
                format!("List {}", basic_type.to_elm())
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

        format!(
            "\
            module {name} exposing ({exports})\n\
            \n\
            import Json.Decode\n\
            import Json.Decode.Extra\n\
            import Json.Decode.Pipeline\n\
            import Json.Encode\n\
            import Json.Encode.Extra\n\
            \n\
            {types}",
            name = self.module,
            exports = exports_str,
            types = types_str
        )
    }
}

impl ElmTyper for TypeSpec {
    fn to_elm(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let sep = format!("\n\t, ");

                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_elm())
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    type alias {name} =\n\
                    \t{{ {fields}\n\
                    \t}}",
                    name = name,
                    fields = fields_fmt,
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

                let sep = "\n\t| ";

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
                    .join(sep);

                format!(
                    "\
                    {subtypes}\
                    type {name}\n\
                    \t= {variants}",
                    subtypes = subtypes,
                    name = name,
                    variants = variants_fmt,
                )
            }
        }
    }

    fn to_elm_decoder(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let sep = format!("\n\t\t");

                let field_decoders = fields
                    .iter()
                    .map(|field| format!("|> {}", field.to_elm_decoder()))
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    decode{name} : Json.Decode.Decoder {name}\n\
                    decode{name} =\n\
                    \tJson.Decode.succeed {name}\n\
                    \t\t{fields}",
                    name = name,
                    fields = field_decoders
                )
            }
            Self::Enum { name, variants } => {
                let sep = format!("\n\t\t, ");

                let variant_decoders = variants
                    .iter()
                    .map(|var| if let EnumVariantData::Struct(_) = &var.data {
                        format!(
                            "Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"{name}\") <|\n\
                            \t\t\tJson.Decode.map {name} (Json.Decode.field \"vardata\" <| decode{parent}{name})",
                            name = var.name,
                            parent = name,
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
                    \tJson.Decode.oneOf\n\
                    \t\t[ {variants}\n\
                    \t\t]",
                    name = name,
                    variants = variant_decoders
                )
            }
        }
    }

    fn to_elm_encoder(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let sep = format!("\n\t\t, ");

                let field_encoders = fields
                    .iter()
                    .map(|field| field.to_elm_encoder())
                    .collect::<Vec<_>>()
                    .join(&sep);

                format!(
                    "\
                    encode{name} : {name} -> Json.Encode.Value\n\
                    encode{name} record =\n\
                    \tJson.Encode.object\n\
                    \t\t[ {fields}\n\
                    \t\t]",
                    name = name,
                    fields = field_encoders
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
                    \tcase var of{variants}",
                    name = name,
                    variants = variant_cases
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
        let elm_type = self.data.to_elm();

        format!(
            "Json.Decode.Pipeline.required \"{name}\" {decoder}",
            name = self.name,
            decoder = elm_json_decoder(&elm_type)
        )
    }

    fn to_elm_encoder(&self) -> String {
        let elm_type = self.data.to_elm();

        format!(
            "(\"{name}\", {encoder} <| record.{name})",
            name = self.name,
            encoder = elm_json_encoder(&elm_type)
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
                \t\t\tJson.Decode.succeed {name}",
                name = self.name,
            ),
            EnumVariantData::Single(api_type) => {
                let elm_type = api_type.to_elm();
                format!(
                    "Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"{name}\") <|\n\
                    \t\t\tJson.Decode.map {name} (Json.Decode.field \"vardata\" <| {decoder})",
                    name = self.name,
                    decoder = elm_json_decoder(&elm_type),
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
                \t\t{name} ->\n\
                \t\t\tJson.Encode.object\n\
                \t\t\t\t[ ( \"var\", Json.Encode.string \"{name}\" )\n\
                \t\t\t\t]",
                name = self.name
            ),
            EnumVariantData::Single(api_type) => {
                let elm_type = api_type.to_elm();
                format!(
                    "\n\
                    \t\t{name} value ->\n\
                    \t\t\tJson.Encode.object\n\
                    \t\t\t\t[ ( \"var\", Json.Encode.string \"{name}\" )\n\
                    \t\t\t\t, ( \"vardata\", {encoder} <| value )\n\
                    \t\t\t\t]",
                    name = self.name,
                    encoder = elm_json_encoder(&elm_type)
                )
            }
            EnumVariantData::Struct(fields) => format!(
                "\n\
                \t\t{name} record ->\n\
                \t\t\tJson.Encode.object\n\
                \t\t\t\t[ ( \"var\", Json.Encode.string \"{name}\" )\n\
                \t\t\t\t, ( \"vardata\", Json.Encode.object\n\
                \t\t\t\t\t[{encoder}\n\
                \t\t\t\t\t] )\n\
                \t\t\t\t]",
                name = self.name,
                encoder = fields
                    .iter()
                    .map(|field| format!(
                        " ( \"{name}\", {encoder} <| record.{name} )",
                        name = field.name,
                        encoder = elm_json_encoder(&field.data.to_elm())
                    ))
                    .collect::<Vec<_>>()
                    .join("\n\t\t\t\t\t,")
            ),
        }
    }
}

fn elm_json_decoder(elm_type: &str) -> String {
    let supported_types = ["String", "Int", "Float", "Bool", "List"];

    let decoders = elm_type
        .split(' ')
        .map(|t| {
            if supported_types.contains(&t) {
                format!("Json.Decode.{}", t.to_lowercase())
            } else if t == "Maybe" {
                String::from("Json.Decode.nullable")
            } else {
                format!("decode{}", t)
            }
        })
        .collect::<Vec<_>>();

    if decoders.len() > 1 {
        format!("({})", decoders.join(" "))
    } else {
        decoders.join(" ")
    }
}

fn elm_json_encoder(elm_type: &str) -> String {
    let supported_types = ["String", "Int", "Float", "Bool", "List"];

    elm_type
        .split(' ')
        .map(|t| {
            if supported_types.contains(&t) {
                format!("Json.Encode.{}", t.to_lowercase())
            } else if t == "Maybe" {
                String::from("Json.Encode.Extra.maybe")
            } else {
                format!("encode{}", t)
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
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
        let expected = "\
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
\t{ foo : Int
\t, bar : String
\t}

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
\tJson.Decode.succeed TestStruct
\t\t|> Json.Decode.Pipeline.required \"foo\" Json.Decode.int
\t\t|> Json.Decode.Pipeline.required \"bar\" Json.Decode.string

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
\tJson.Encode.object
\t\t[ (\"foo\", Json.Encode.int <| record.foo)
\t\t, (\"bar\", Json.Encode.string <| record.bar)
\t\t]";

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
        let expected = "\
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
\t{ foo : (List Int)
\t}

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
\tJson.Decode.succeed TestStruct
\t\t|> Json.Decode.Pipeline.required \"foo\" (Json.Decode.list Json.Decode.int)

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
\tJson.Encode.object
\t\t[ (\"foo\", Json.Encode.list Json.Encode.int <| record.foo)
\t\t]";

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
        let expected = "\
module TestType exposing (TestStruct, decodeTestStruct, encodeTestStruct)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestStruct =
\t{ foo : (Maybe Int)
\t}

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
\tJson.Decode.succeed TestStruct
\t\t|> Json.Decode.Pipeline.required \"foo\" (Json.Decode.nullable Json.Decode.int)

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
\tJson.Encode.object
\t\t[ (\"foo\", Json.Encode.Extra.maybe Json.Encode.int <| record.foo)
\t\t]";

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
        let expected = "\
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type TestEnum
\t= Foo
\t| Bar
\t| Qux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
\tJson.Decode.oneOf
\t\t[ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Foo\") <|
\t\t\tJson.Decode.succeed Foo
\t\t, Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
\t\t\tJson.Decode.succeed Bar
\t\t, Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
\t\t\tJson.Decode.succeed Qux
\t\t]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
\tcase var of
\t\tFoo ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Foo\" )
\t\t\t\t]
\t\tBar ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Bar\" )
\t\t\t\t]
\t\tQux ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Qux\" )
\t\t\t\t]";

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
        let expected = "\
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestEnumQux =
\t{ sub1 : Int
\t, sub2 : String
\t}

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
\tJson.Decode.succeed TestEnumQux
\t\t|> Json.Decode.Pipeline.required \"sub1\" Json.Decode.int
\t\t|> Json.Decode.Pipeline.required \"sub2\" Json.Decode.string

type TestEnum
\t= Foo
\t| Bar Bool
\t| Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
\tJson.Decode.oneOf
\t\t[ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Foo\") <|
\t\t\tJson.Decode.succeed Foo
\t\t, Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
\t\t\tJson.Decode.map Bar (Json.Decode.field \"vardata\" <| Json.Decode.bool)
\t\t, Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
\t\t\tJson.Decode.map Qux (Json.Decode.field \"vardata\" <| decodeTestEnumQux)
\t\t]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
\tcase var of
\t\tFoo ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Foo\" )
\t\t\t\t]
\t\tBar value ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Bar\" )
\t\t\t\t, ( \"vardata\", Json.Encode.bool <| value )
\t\t\t\t]
\t\tQux record ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Qux\" )
\t\t\t\t, ( \"vardata\", Json.Encode.object
\t\t\t\t\t[ ( \"sub1\", Json.Encode.int <| record.sub1 )
\t\t\t\t\t, ( \"sub2\", Json.Encode.string <| record.sub2 )
\t\t\t\t\t] )
\t\t\t\t]";

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
        let expected = "\
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestEnumQux =
\t{ sub1 : (List Bool)
\t}

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
\tJson.Decode.succeed TestEnumQux
\t\t|> Json.Decode.Pipeline.required \"sub1\" (Json.Decode.list Json.Decode.bool)

type TestEnum
\t= Bar (List Int)
\t| Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
\tJson.Decode.oneOf
\t\t[ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
\t\t\tJson.Decode.map Bar (Json.Decode.field \"vardata\" <| (Json.Decode.list Json.Decode.int))
\t\t, Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
\t\t\tJson.Decode.map Qux (Json.Decode.field \"vardata\" <| decodeTestEnumQux)
\t\t]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
\tcase var of
\t\tBar value ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Bar\" )
\t\t\t\t, ( \"vardata\", Json.Encode.list Json.Encode.int <| value )
\t\t\t\t]
\t\tQux record ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Qux\" )
\t\t\t\t, ( \"vardata\", Json.Encode.object
\t\t\t\t\t[ ( \"sub1\", Json.Encode.list Json.Encode.bool <| record.sub1 )
\t\t\t\t\t] )
\t\t\t\t]";

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
        let expected = "\
module TestType exposing (TestEnum(..), decodeTestEnum, encodeTestEnum)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra

type alias TestEnumQux =
\t{ sub1 : (Maybe Bool)
\t}

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
\tJson.Decode.succeed TestEnumQux
\t\t|> Json.Decode.Pipeline.required \"sub1\" (Json.Decode.nullable Json.Decode.bool)

type TestEnum
\t= Bar (Maybe Int)
\t| Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
\tJson.Decode.oneOf
\t\t[ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
\t\t\tJson.Decode.map Bar (Json.Decode.field \"vardata\" <| (Json.Decode.nullable Json.Decode.int))
\t\t, Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
\t\t\tJson.Decode.map Qux (Json.Decode.field \"vardata\" <| decodeTestEnumQux)
\t\t]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
\tcase var of
\t\tBar value ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Bar\" )
\t\t\t\t, ( \"vardata\", Json.Encode.Extra.maybe Json.Encode.int <| value )
\t\t\t\t]
\t\tQux record ->
\t\t\tJson.Encode.object
\t\t\t\t[ ( \"var\", Json.Encode.string \"Qux\" )
\t\t\t\t, ( \"vardata\", Json.Encode.object
\t\t\t\t\t[ ( \"sub1\", Json.Encode.Extra.maybe Json.Encode.bool <| record.sub1 )
\t\t\t\t\t] )
\t\t\t\t]";

        compare_strings(expected, create_spec_enum_with_option().to_elm());
    }
}
