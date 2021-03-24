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
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
\tpub foo: u32,
\tpub bar: String,
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
                    data: ApiType::Array(BasicApiType::Uint),
                }],
            }],
        }
    }

    #[test]
    fn rust_struct_with_vec() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
\tpub foo: Vec<u32>,
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
                    data: ApiType::Option(BasicApiType::Uint),
                }],
            }],
        }
    }

    #[test]
    fn rust_struct_with_option() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct TestStruct {
\tpub foo: Option<u32>,
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
    fn rust_enum_simple() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = \"var\", content = \"vardata\")]
pub enum TestEnum {
\tFoo,
\tBar,
\tQux,
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
    fn rust_enum_complex() {
        let expected = "\
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
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
                        data: EnumVariantData::Single(ApiType::Array(BasicApiType::Uint)),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![EnumStructField {
                            name: "sub1".into(),
                            data: ApiType::Array(BasicApiType::Bool),
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
\tBar(Vec<u32>),
\tQux {
\t\tsub1: Vec<bool>,
\t},
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
                        data: EnumVariantData::Single(ApiType::Option(BasicApiType::Uint)),
                    },
                    EnumVariant {
                        name: "Qux".into(),
                        data: EnumVariantData::Struct(vec![EnumStructField {
                            name: "sub1".into(),
                            data: ApiType::Option(BasicApiType::Bool),
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
\tBar(Option<u32>),
\tQux {
\t\tsub1: Option<bool>,
\t},
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
