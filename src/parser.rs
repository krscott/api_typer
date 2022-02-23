use std::collections::HashMap;

use nom::{
    bytes::complete::tag,
    character::complete::{alphanumeric0, anychar, multispace0, multispace1, none_of},
    combinator::{not, opt},
    error::{context, ErrorKind, VerboseError},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    AsChar, IResult, InputTakeAtPosition,
};

type Res<T, U> = IResult<T, U, VerboseError<T>>;

// pub fn spec(input: &str) -> nom::IResult<&str, crate::spec::ApiSpec> {
//     let (input, _) = nom::bytes::complete::tag("module")(input.trim_start())?;

// }

pub fn token<'a, O, E: nom::error::ParseError<&'a str>, F>(
    f: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: nom::Parser<&'a str, O, E>,
{
    preceded(multispace0, f)
}

fn ident(input: &str) -> Res<&str, &str> {
    input.split_at_position1_complete(|c| !c.is_alphanum() && c != '_', ErrorKind::Fail)
}

fn quote_string(input: &str) -> Res<&str, &str> {
    fn until_quote(input: &str) -> Res<&str, &str> {
        input.split_at_position_complete(|c| c == '"')
    }

    delimited(tag("\""), until_quote, tag("\""))(input)
}

fn expr_module(input: &str) -> Res<&str, &str> {
    context(
        "module",
        terminated(
            preceded(token(tag("module")), token(ident)),
            token(tag(";")),
        ),
    )(input)
}

fn expr_alias(input: &str) -> Res<&str, (&str, HashMap<String, String>)> {
    let (remaining, (_, id, pairs)) = context(
        "alias",
        tuple((
            token(tag("alias")),
            token(ident),
            delimited(
                token(tag("{")),
                separated_list0(
                    token(tag(",")),
                    separated_pair(token(ident), token(tag(":")), token(quote_string)),
                ),
                preceded(opt(token(tag(","))), token(tag("}"))),
            ),
        )),
    )(input)?;

    let pairs = pairs
        .into_iter()
        .map(|(a, b)| (a.into(), b.into()))
        .collect();

    Ok((remaining, (id, pairs)))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_string() {
        assert_eq!(quote_string(r#""abc""#), Ok(("", "abc")));
        assert_eq!(quote_string(r#""""#), Ok(("", "")));
        assert_eq!(
            quote_string(r#""Hello, World!""#),
            Ok(("", "Hello, World!"))
        );
        assert_eq!(
            quote_string(r#""inside" outside"#),
            Ok((" outside", "inside"))
        );
    }

    #[test]
    fn test_module() {
        assert_eq!(expr_module("module \r\n my_test;"), Ok(("", "my_test")));
    }

    #[test]
    fn test_alias() {
        let spec = r#"
alias ArcBit {
    rs: "Arc<bool>",
    ts: "boolean",
    elm: "Bool",
}"#;

        let mut expected = HashMap::new();
        expected.insert("rs".into(), "Arc<bool>".into());
        expected.insert("ts".into(), "boolean".into());
        expected.insert("elm".into(), "Bool".into());

        assert_eq!(expr_alias(spec), Ok(("", ("ArcBit", expected))));
    }

    #[test]
    fn test_spec() {
        let spec = r#"
module test_spec;

alias ArcBit {
    rs: "Arc<bool>",
    ts: "boolean",
    elm: "Bool",
}

enum TestEnum {
    Foo,
    Bar(bool),
    Qux {
        sub1: u32,
        sub2: String,
    },
}

struct MyStruct {
    num: i32,
    arr: Vec<String>,
    maybe: Option<f32>,
    myenum: TestEnum,
    arcbit: ArcBit,
}"#;

        // match parse_module(spec) {
        //     Ok(x) => eprintln!("{:?}", x),
        //     Err(e) => eprintln!("{}", e),
        // }

        // assert!(false);
    }
}
