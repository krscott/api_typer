use crate::spec::*;

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
