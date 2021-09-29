use std::collections::HashMap;

use serde::{Deserialize, Serialize};

pub const INDENT: &str = "    ";

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum BasicApiType {
    String,
    Int,
    Uint,
    Float,
    Double,
    Bool,
    Custom(String),
    Recursive(String),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum ComplexApiType {
    Option(Box<ApiType>),
    Array(Box<ApiType>),
    Map(BasicApiType, Box<ApiType>),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum ApiType {
    Basic(BasicApiType),
    Complex(ComplexApiType),
}

impl ApiType {
    pub fn basic(basic_type: BasicApiType) -> Self {
        Self::Basic(basic_type)
    }

    pub fn option(basic_type: BasicApiType) -> Self {
        Self::Complex(ComplexApiType::Option(Box::new(Self::Basic(basic_type))))
    }

    pub fn array(basic_type: BasicApiType) -> Self {
        Self::Complex(ComplexApiType::Array(Box::new(Self::Basic(basic_type))))
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(untagged)]
pub enum EnumVariantData {
    None,
    Single(ApiType),
    Struct(Vec<EnumStructField>),
}

impl Default for EnumVariantData {
    fn default() -> Self {
        EnumVariantData::None
    }
}

impl EnumVariantData {
    fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            _ => false,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct StructField {
    pub name: String,
    pub data: ApiType,
}

impl From<EnumStructField> for StructField {
    fn from(other: EnumStructField) -> Self {
        let EnumStructField { name, data } = other;
        Self { name, data }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct EnumStructField {
    pub name: String,
    pub data: ApiType,
}

impl From<StructField> for EnumStructField {
    fn from(other: StructField) -> Self {
        let StructField { name, data } = other;
        Self { name, data }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct EnumVariant {
    pub name: String,
    #[serde(default, skip_serializing_if = "EnumVariantData::is_none")]
    pub data: EnumVariantData,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum TypeSpec {
    Struct {
        name: String,
        fields: Vec<StructField>,
    },
    Enum {
        name: String,
        variants: Vec<EnumVariant>,
    },
}

#[derive(Debug, Default, Serialize, Deserialize, Clone)]
pub struct ApiSpec {
    pub module: String,

    #[serde(default)]
    pub define_custom: HashMap<String, HashMap<String, String>>,

    pub types: Vec<TypeSpec>,
}

impl ApiSpec {
    pub fn get_custom_type<S: AsRef<str>>(&self, type_str: S, lang: &str) -> String {
        self.define_custom
            .get(type_str.as_ref())
            .and_then(|map| map.get(lang))
            .map(|s| s.clone())
            .unwrap_or_else(|| type_str.as_ref().to_owned())
    }
}
