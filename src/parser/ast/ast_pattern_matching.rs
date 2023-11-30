use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::lexer::{Punct, SrcPos, SrcToken, Token};
use crate::parser::ast::expression::AstExpression;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::literal::{AstBoolLiteral, AstCharLiteral, AstNumberLiteral, AstStringLiteral};
use crate::parser::ast::name::AstNameExpr;


#[derive(Debug, Clone)]
enum AstPattern {
    Value { expr: AstExpression, pos: SrcPos },
    Or { members: Vec<AstPattern>, pos: SrcPos },
    NamedStruct { ty: AstType, params: HashMap<String, AstPattern>, pos: SrcPos },
    ListedStruct { ty: AstType, params: Vec<AstPattern>, pos: SrcPos },
}

impl Display for AstPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstPattern::Value { expr, pos } => {
                write!(f, "{expr}")?;
            }
            AstPattern::Or { members, pos } => {
                let mut itr = members.iter();
                if let Some(item) = itr.next() {
                    write!(f, "{item}")?;
                    for item in itr {
                        write!(f, " | {item}")?;
                    }
                }
            }
            AstPattern::NamedStruct { ty, params, pos } => {
                write!(f, "{ty} {{")?;

                let mut itr = params.iter();
                if let Some((name, item)) = itr.next() {
                    writeln!(f)?;
                    writeln!(f, "    {name}: {item},")?;
                    for (name, item) in itr {
                        writeln!(f, "    {name}: {item},")?;
                    }
                }

                write!(f, "}}")?;
            }
            AstPattern::ListedStruct { ty, params, pos } => {
                write!(f, "{ty}")?;

                let mut itr = params.iter();
                if let Some(item) = itr.next() {
                    write!(f, " ({item}")?;
                    for item in itr {
                        write!(f, ", {item}")?;
                    }
                    write!(f, ")")?;
                }
            }
        }
        Ok(())
    }
}

impl AstPattern {
    fn parse_primary(parser: &mut Parser) -> Result<Self, ParseError> {
        // if auto dereference is implemented someday, that should go here since it is effectively
        // a prefix operator

        // check main
        let x = match parser.peak() {
            Ok(SrcToken { token: Token::NumLiteral(_, _, _, _), pos }) => {
                let pos = *pos;
                Ok(AstPattern::Value { expr: AstNumberLiteral::parse(parser)?, pos })
            },
            Ok(SrcToken { token: Token::StrLiteral(_), pos }) => {
                let pos = *pos;
                Ok(AstPattern::Value { expr: AstStringLiteral::parse(parser)?, pos })
            },
            Ok(SrcToken { token: Token::CharLiteral(_), pos }) => {
                let pos = *pos;
                Ok(AstPattern::Value { expr: AstCharLiteral::parse(parser)?, pos })
            },
            Ok(SrcToken { token: Token::Punct(Punct::BracketOpen), pos }) => {
                let val = AstPattern::parse(parser)?;
                expect_token!(parser; (Token::Punct(Punct::BracketClose))
                    expected ")")?;
                Ok(val)
            },
            Ok(SrcToken { token: Token::Ident(ident), pos })
            if ident == "false" || ident == "true" => {
                let pos = *pos;
                Ok(AstPattern::Value { expr: AstBoolLiteral::parse(parser)?, pos })
            },
            Ok(SrcToken { token: Token::Ident(_), pos }) => {
                let pos = *pos;
                let x = AstNameExpr::parse(parser)?;
                if parser.env.find_struct(&x).is_some() {
                    match parser.peak() {
                        Ok(local!(Token::Punct(Punct::BracketOpen | Punct::BraceOpen))) => {
                            Self::parse_init(parser, x.into())
                        },
                        _ => Ok(AstPattern::ListedStruct { ty: x.into(), params: vec![], pos })
                    }
                } else {
                    Ok(AstPattern::Value { expr: x.into(), pos })
                }
            }
            Ok(_) => Err(ParseError::UnexpectedToken(
                Box::new(parser.next().unwrap()),
                "primary match expression".to_owned(),
            )),
            Err(e) => Err(ParseError::LexError(e.clone()))
        }?;

        Ok(x)
    }

    fn parse_init(parser: &mut Parser, ty: AstType) -> Result<Self, ParseError> {
        match parser.next() {
            // parse named parameter list
            Ok(SrcToken { token: (Token::Punct(Punct::BraceOpen)), pos }) => {
                // parse with named parameters
                let mut params = HashMap::<String, AstPattern>::new();
                loop {
                    // check for closing bracket
                    if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                        parser.next()?;
                        break;
                    }
                    let (name, pos) = expect_token!(parser; (Token::Ident(name)),
                        pos => (name.clone(), pos) expected "parameter name")?;

                    // check for value expression
                    if let Ok(local!(Token::Punct(Punct::Colon))) = parser.peak() {
                        parser.next()?;
                        let val = Self::parse(parser)?;
                        params.insert(name, val);
                    } else {
                        let val = AstPattern::Value {
                            expr: AstNameExpr::var(name.clone()),
                            pos,
                        };
                        params.insert(name, val);
                    }

                    // check if next entry is available
                    match parser.next() {
                        Ok(local!(Token::Punct(Punct::Comma))) => {
                            Ok(())
                        },
                        Ok(local!(Token::Punct(Punct::BraceClose))) => {
                            break;
                        },
                        Ok(t) => Err(ParseError::UnexpectedToken(
                            Box::new(t),
                            "comma `,` or closing brace `}` in named pattern init list".to_owned()
                        )),
                        Err(e) => Err(ParseError::LexError(e)),
                    }?;
                }

                Ok(AstPattern::NamedStruct {
                    ty,
                    params,
                    pos
                })
            },
            // parse order-dependent parameter list
            Ok(SrcToken { token: Token::Punct(Punct::BracketOpen), pos }) => {
                // parse with order-dependent parameters
                let params = Self::parse_ord_dependent_unchecked(parser)?;
                Ok(AstPattern::ListedStruct {
                    ty,
                    params,
                    pos
                })
            },
            Ok(e) => Err(ParseError::UnexpectedToken(
                Box::new(e),
                "struct init in pattern matching value with `(` or `{`".to_owned(),
            )),
            Err(e) => Err(ParseError::LexError(e)),
        }
    }

    fn parse_ord_dependent_unchecked(
        parser: &mut Parser
    ) -> Result<Vec<AstPattern>, ParseError> {
        // parse with order-dependent parameters
        let mut params = Vec::<AstPattern>::new();
        loop {
            // check for closing bracket
            if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                parser.next()?;
                break;
            }
            let val = Self::parse(parser)?;
            params.push(val);

            // check if next entry is available
            match parser.next() {
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    Ok(())
                },
                Ok(local!(Token::Punct(Punct::BracketClose))) => {
                    break;
                },
                Ok(t) => Err(ParseError::UnexpectedToken(
                    Box::new(t),
                    "comma `,` or closing bracket `)` expected in order dependent \
                    parameter list".to_owned()
                )),
                Err(e) => Err(ParseError::LexError(e)),
            }?;
        }
        Ok(params)
    }
}


impl Parsable for AstPattern {
    type Output = AstPattern;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        // parse primary and check for `|` to find or cases
        let primary = AstPattern::parse_primary(parser)?;
        if let Ok(local!(Token::Punct(Punct::Or))) = parser.peak() {
            let pos = parser.next()?.pos;
            // collect ors in a vector
            let mut ors = Vec::new();
            ors.push(primary);

            loop {
                ors.push(AstPattern::parse_primary(parser)?);
                // check if there is a next `|`
                if let Ok(local!(Token::Punct(Punct::Or))) = parser.peak() {
                    parser.next()?;
                } else {
                    break;
                }
            }
            Ok(AstPattern::Or {
                members: ors,
                pos,
            })
        } else {
            Ok(primary)
        }
    }
}



#[cfg(test)]
mod test {
    use crate::lexer::SrcPos;
    use crate::parser::{ParseError, Parser};
    use crate::parser::ast::ast_pattern_matching::AstPattern;
    use crate::parser::ast::ast_type::TypeName;
    use crate::parser::ast::Parsable;

    #[test]
    fn test_parser() -> Result<(), ParseError> {
        let src = r#"
Some(1 | 3 | 5)
MyData { expl: "hello, world!", c: 'a', }
None =
        "#;
        let mut parser = Parser::new(src);
        parser.env.push();
        parser.env.push_struct(
            TypeName::from(vec!["Option".to_string(), "Some".to_string()]),
            SrcPos::default()
        )?;
        parser.env.push_struct(
            TypeName::from(vec!["Option".to_string(), "None".to_string()]),
            SrcPos::default()
        )?;
        parser.env.push_struct(
            TypeName::from(vec!["MyData".to_string()]),
            SrcPos::default()
        )?;

        let pattern = AstPattern::parse(&mut parser)?;
        println!("{pattern}");
        let pattern = AstPattern::parse(&mut parser)?;
        println!("{pattern}");
        let pattern = AstPattern::parse(&mut parser)?;
        println!("{pattern}");

        Ok(())
    }
}
