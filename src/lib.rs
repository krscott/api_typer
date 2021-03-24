mod elm_typer;
mod rust_typer;
mod spec;

pub use elm_typer::ElmTyper;
pub use rust_typer::RustTyper;
pub use spec::*;

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
                        data: ("u32".into(), "Int".into()),
                    },
                    StructField {
                        name: "bar".into(),
                        data: ("String".into(), "String".into()),
                    },
                ],
            }],
        }
    }

    #[test]
    fn rust_struct_simple() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
    pub foo: u32,
    pub bar: String,
}";

        compare_strings(expected, create_spec_struct_simple().to_rust());
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
    { foo : Int
    , bar : String
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required \"foo\" Json.Decode.int
        |> Json.Decode.Pipeline.required \"bar\" Json.Decode.string

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ (\"foo\", Json.Encode.int <| record.foo)
        , (\"bar\", Json.Encode.string <| record.bar)
        ]";

        compare_strings(expected, create_spec_struct_simple().to_elm());
    }

    fn create_spec_struct_with_vec() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "foo".into(),
                    data: ("Vec<u32>".into(), "List Int".into()),
                }],
            }],
        }
    }

    #[test]
    fn rust_struct_with_vec() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
    pub foo: Vec<u32>,
}";

        compare_strings(expected, create_spec_struct_with_vec().to_rust());
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
    { foo : (List Int)
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required \"foo\" (Json.Decode.list Json.Decode.int)

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ (\"foo\", Json.Encode.list Json.Encode.int <| record.foo)
        ]";

        compare_strings(expected, create_spec_struct_with_vec().to_elm());
    }

    fn create_spec_struct_with_option() -> ApiSpec {
        ApiSpec {
            module: "TestType".into(),
            types: vec![TypeSpec::Struct {
                name: "TestStruct".into(),
                fields: vec![StructField {
                    name: "foo".into(),
                    data: ("Option<u32>".into(), "Maybe Int".into()),
                }],
            }],
        }
    }

    #[test]
    fn rust_struct_with_option() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
    pub foo: Option<u32>,
}";

        compare_strings(expected, create_spec_struct_with_option().to_rust());
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
    { foo : (Maybe Int)
    }

decodeTestStruct : Json.Decode.Decoder TestStruct
decodeTestStruct =
    Json.Decode.succeed TestStruct
        |> Json.Decode.Pipeline.required \"foo\" (Json.Decode.nullable Json.Decode.int)

encodeTestStruct : TestStruct -> Json.Encode.Value
encodeTestStruct record =
    Json.Encode.object
        [ (\"foo\", Json.Encode.Extra.maybe Json.Encode.int <| record.foo)
        ]";

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
    fn rust_enum_simple() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
    Foo,
    Bar,
    Qux,
}";

        compare_strings(expected, create_spec_enum_simple().to_rust());
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
    = Foo
    | Bar
    | Qux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Foo\") <|
            Json.Decode.succeed Foo
        , Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
            Json.Decode.succeed Bar
        , Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
            Json.Decode.succeed Qux
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Foo ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Foo\" )
                ]
        Bar ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Bar\" )
                ]
        Qux ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Qux\" )
                ]";

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
                        data: EnumVariantData::Single(("bool".into(), "Bool".into())),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![
                            EnumStructField {
                                name: "sub1".into(),
                                data: ("u32".into(), "Int".into()),
                            },
                            EnumStructField {
                                name: "sub2".into(),
                                data: ("String".into(), "String".into()),
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
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
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
    { sub1 : Int
    , sub2 : String
    }

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
    Json.Decode.succeed TestEnumQux
        |> Json.Decode.Pipeline.required \"sub1\" Json.Decode.int
        |> Json.Decode.Pipeline.required \"sub2\" Json.Decode.string

type TestEnum
    = Foo
    | Bar Bool
    | Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Foo\") <|
            Json.Decode.succeed Foo
        , Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
            Json.Decode.map Bar (Json.Decode.field \"vardata\" <| Json.Decode.bool)
        , Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
            Json.Decode.map Qux (Json.Decode.field \"vardata\" <| decodeTestEnumQux)
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Foo ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Foo\" )
                ]
        Bar value ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Bar\" )
                , ( \"vardata\", Json.Encode.bool <| value )
                ]
        Qux record ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Qux\" )
                , ( \"vardata\", Json.Encode.object
                    [ ( \"sub1\", Json.Encode.int <| record.sub1 )
                    , ( \"sub2\", Json.Encode.string <| record.sub2 )
                    ] )
                ]";

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
                        data: EnumVariantData::Single(("Vec<u32>".into(), "List Int".into())),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![EnumStructField {
                            name: "sub1".into(),
                            data: ("Vec<bool>".into(), "List Bool".into()),
                        }]),
                    },
                ],
            }],
        }
    }

    #[test]
    fn rust_enum_with_vec() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
    Bar(Vec<u32>),
    Qux {
        sub1: Vec<bool>,
    },
}";

        compare_strings(expected, create_spec_enum_with_vec().to_rust());
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
    { sub1 : (List Bool)
    }

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
    Json.Decode.succeed TestEnumQux
        |> Json.Decode.Pipeline.required \"sub1\" (Json.Decode.list Json.Decode.bool)

type TestEnum
    = Bar (List Int)
    | Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
            Json.Decode.map Bar (Json.Decode.field \"vardata\" <| (Json.Decode.list Json.Decode.int))
        , Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
            Json.Decode.map Qux (Json.Decode.field \"vardata\" <| decodeTestEnumQux)
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Bar value ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Bar\" )
                , ( \"vardata\", Json.Encode.list Json.Encode.int <| value )
                ]
        Qux record ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Qux\" )
                , ( \"vardata\", Json.Encode.object
                    [ ( \"sub1\", Json.Encode.list Json.Encode.bool <| record.sub1 )
                    ] )
                ]";

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
                        data: EnumVariantData::Single(("Option<u32>".into(), "Maybe Int".into())),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![EnumStructField {
                            name: "sub1".into(),
                            data: ("Option<bool>".into(), "Maybe Bool".into()),
                        }]),
                    },
                ],
            }],
        }
    }

    #[test]
    fn rust_enum_with_option() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
    Bar(Option<u32>),
    Qux {
        sub1: Option<bool>,
    },
}";

        compare_strings(expected, create_spec_enum_with_option().to_rust());
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
    { sub1 : (Maybe Bool)
    }

decodeTestEnumQux : Json.Decode.Decoder TestEnumQux
decodeTestEnumQux =
    Json.Decode.succeed TestEnumQux
        |> Json.Decode.Pipeline.required \"sub1\" (Json.Decode.nullable Json.Decode.bool)

type TestEnum
    = Bar (Maybe Int)
    | Qux TestEnumQux

decodeTestEnum : Json.Decode.Decoder TestEnum
decodeTestEnum =
    Json.Decode.oneOf
        [ Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Bar\") <|
            Json.Decode.map Bar (Json.Decode.field \"vardata\" <| (Json.Decode.nullable Json.Decode.int))
        , Json.Decode.Extra.when (Json.Decode.field \"var\" Json.Decode.string) ((==) \"Qux\") <|
            Json.Decode.map Qux (Json.Decode.field \"vardata\" <| decodeTestEnumQux)
        ]

encodeTestEnum : TestEnum -> Json.Encode.Value
encodeTestEnum var =
    case var of
        Bar value ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Bar\" )
                , ( \"vardata\", Json.Encode.Extra.maybe Json.Encode.int <| value )
                ]
        Qux record ->
            Json.Encode.object
                [ ( \"var\", Json.Encode.string \"Qux\" )
                , ( \"vardata\", Json.Encode.object
                    [ ( \"sub1\", Json.Encode.Extra.maybe Json.Encode.bool <| record.sub1 )
                    ] )
                ]";

        compare_strings(expected, create_spec_enum_with_option().to_elm());
    }
}
