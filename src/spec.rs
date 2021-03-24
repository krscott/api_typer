use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum EnumVariantData {
    None,
    Single((String, String)),
    Struct(Vec<EnumStructField>),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct StructField {
    pub name: String,
    pub data: (String, String),
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
    pub data: (String, String),
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ApiSpec {
    pub module: String,
    pub types: Vec<TypeSpec>,
}
