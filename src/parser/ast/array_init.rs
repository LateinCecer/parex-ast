use std::fmt::{Display, Formatter};
use crate::lexer::{Punct, Token};
use crate::parser::ast::expression::AstExpression;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};


#[derive(Clone, Debug, PartialEq)]
enum ArrayParams {
    List(Vec<AstExpression>),
    Copy(Box<AstExpression>, Box<AstExpression>),
}

impl Display for ArrayParams {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        match self {
            Self::List(list) if list.is_empty() => (),
            Self::List(list) => {
                let mut iter = list.iter();
                write!(f, "{}", iter.next().unwrap())?;

                loop {
                    if let Some(e) = iter.next() {
                        write!(f, ", {}", e)?;
                    } else {
                        break;
                    }
                }
            },
            Self::Copy(val, num) => {
                write!(f, "{}; {}", val, num)?;
            }
        }
        write!(f, "]")
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct AstArrayInit {
    params: ArrayParams,
}

impl Parsable for AstArrayInit {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::SquareOpen))
            expected "array init list started by `[`")?;
        let first = AstExpression::parse(parser)?;

        match parser.next() {
            Ok(local!(Token::Punct(Punct::Semicolon))) => {
                // find amount expression
                let amount = AstExpression::parse(parser)?;
                expect_token!(parser; (Token::Punct(Punct::SquareClose))
                    expected "array init closing `]`")?;

                Ok(AstArrayInit {
                    params: ArrayParams::Copy(
                        Box::new(first),
                        Box::new(amount))
                }.into())
            },
            Ok(local!(Token::Punct(Punct::Comma))) => {
                // loop to find other entries
                let mut vec = vec![first];
                loop {
                    if let Ok(local!(Token::Punct(Punct::SquareClose))) = parser.peak() {
                        parser.next()?;
                        break;
                    }
                    vec.push(AstExpression::parse(parser)?);

                    // check if next entry is available
                    match parser.next() {
                        Ok(local!(Token::Punct(Punct::Comma))) => {
                            Ok(())
                        },
                        Ok(local!(Token::Punct(Punct::SquareClose))) => {
                            break;
                        },
                        Ok(t) => Err(ParseError::UnexpectedToken(
                            Box::new(t),
                            "comma `,` or closing square bracket `]`".to_owned()
                        )),
                        Err(e) => Err(ParseError::LexError(e)),
                    }?;
                }

                Ok(AstArrayInit {
                    params: ArrayParams::List(vec),
                }.into())
            }
            Ok(e) => Err(ParseError::UnexpectedToken(
                Box::new(e),
                "`;` for array copy initializer or `,` for array list initializer".to_owned(),
            )),
            Err(e) => Err(ParseError::LexError(e)),
        }
    }
}

impl From<AstArrayInit> for AstExpression {
    fn from(value: AstArrayInit) -> Self {
        AstExpression::ArrayInit(value)
    }
}

impl Display for AstArrayInit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[]")
    }
}



#[cfg(test)]
mod test {
    use crate::parser::ast::array_init::{ArrayParams, AstArrayInit};
    use crate::parser::ast::ast_type::AstType;
    use crate::parser::ast::expression::AstExpression;
    use crate::parser::ast::literal::AstNumberLiteral;
    use crate::parser::ast::Parsable;
    use crate::parser::Parser;

    #[test]
    fn test_list() {
        let mut parser = Parser::new("[0, 1, 2, 3, 4]");
        assert_eq!(
            AstExpression::parse(&mut parser),
            Ok(AstExpression::ArrayInit(AstArrayInit {
                params: ArrayParams::List(vec![
                    AstExpression::NumberLit(AstNumberLiteral::Int(0, None)),
                    AstExpression::NumberLit(AstNumberLiteral::Int(1, None)),
                    AstExpression::NumberLit(AstNumberLiteral::Int(2, None)),
                    AstExpression::NumberLit(AstNumberLiteral::Int(3, None)),
                    AstExpression::NumberLit(AstNumberLiteral::Int(4, None)),
                ])
            }))
        );
    }

    #[test]
    fn test_copy() {
        let mut parser = Parser::new("[42_u8; 10]");
        assert_eq!(
            AstExpression::parse(&mut parser),
            Ok(AstExpression::ArrayInit(AstArrayInit {
                params: ArrayParams::Copy(
                    Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(42, Some(AstType::Base("u8".into()))))),
                    Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(10, None))),
                )
            }))
        );
    }
}
