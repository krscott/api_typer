use crate::spec::*;

pub trait TypescriptTyper {
    fn to_typescript(&self) -> String;
}

impl TypescriptTyper for BasicApiType {
    fn to_typescript(&self) -> String {
        match self {
            BasicApiType::Custom(s) => s.clone(),
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
    fn to_typescript(&self) -> String {
        match self {
            ApiType::Basic(basic_type) => basic_type.to_typescript(),
            ApiType::Complex(ComplexApiType::Option(basic_type)) => {
                format!("{} | null", basic_type.to_typescript())
            }
            ApiType::Complex(ComplexApiType::Array(basic_type)) => {
                format!("Array<{}>", basic_type.to_typescript())
            }
        }
    }
}

impl TypescriptTyper for ApiSpec {
    fn to_typescript(&self) -> String {
        self.types
            .iter()
            .map(|t| t.to_typescript())
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

impl TypescriptTyper for TypeSpec {
    fn to_typescript(&self) -> String {
        match self {
            Self::Struct { name, fields } => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_typescript())
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
                let variants_fmt = variants
                    .iter()
                    .map(|var| var.to_typescript())
                    .collect::<Vec<_>>()
                    .join(" |\n\t");

                let variant_objs = variants
                    .iter()
                    .map(|var| {
                        format!(
                            "export const {enumname}{varname}Var = \"{varname}\"",
                            varname = var.name,
                            enumname = name,
                        )
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                let match_func = {
                    let match_arms = variants
                        .iter()
                        .map(|EnumVariant { name, data }| {
                            let vardata_arg = match data {
                                EnumVariantData::None => String::from(""),
                                EnumVariantData::Single(api_type) => {
                                    format!("vardata: {}", api_type.to_typescript())
                                }
                                EnumVariantData::Struct(fields) => {
                                    let fields_ts = fields
                                        .iter()
                                        .map(|field| field.to_typescript())
                                        .collect::<Vec<_>>()
                                        .join("");

                                    format!("vardata: {{\n{}\t}}", fields_ts)
                                }
                            };

                            format!("\t{}: ({}) => T,\n", name, vardata_arg,)
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
                                "\t\tcase \"{name}\": return arms.{name}({x_vardata})\n",
                                name = name,
                                x_vardata = x_vardata
                            )
                        })
                        .collect::<Vec<_>>()
                        .join("");

                    format!(
                        "export function match{name}<T>(x: {name}, arms: {{\n\
                            {match_arms}\
                        }}): T {{\n\
                            \tswitch (x.var) {{\n\
                                {match_cases}\
                            \t}}\n\
                        }}",
                        name = name,
                        match_arms = match_arms,
                        match_cases = match_cases,
                    )
                };

                format!(
                    "export type {name} = (\n\
                    \t{variants}\n\
                    )\n\
                    \n\
                    {objects}\n\
                    \n\
                    {match_func}\n",
                    name = name,
                    variants = variants_fmt,
                    objects = variant_objs,
                    match_func = match_func
                )
            }
        }
    }
}

impl TypescriptTyper for StructField {
    fn to_typescript(&self) -> String {
        format!("\t{}: {},\n", self.name, self.data.to_typescript())
    }
}

impl TypescriptTyper for EnumStructField {
    fn to_typescript(&self) -> String {
        format!("\t\t{}: {},\n", self.name, self.data.to_typescript())
    }
}

impl TypescriptTyper for EnumVariant {
    fn to_typescript(&self) -> String {
        if let EnumVariantData::None = self.data {
            format!("{{ var: \"{}\" }}", self.name)
        } else {
            format!(
                "{{ var: \"{}\", vardata: {} }}",
                self.name,
                self.data.to_typescript()
            )
        }
    }
}

impl TypescriptTyper for EnumVariantData {
    fn to_typescript(&self) -> String {
        match self {
            Self::None => "null".into(),
            Self::Single(api_type) => format!("{}", api_type.to_typescript()),
            Self::Struct(fields) => {
                let fields_fmt = fields
                    .iter()
                    .map(|field| field.to_typescript())
                    .collect::<Vec<_>>()
                    .join("");

                format!(" {{\n{fields}\t}}", fields = fields_fmt)
            }
        }
    }
}
