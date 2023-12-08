use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::ast_type::{AstType};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};


#[derive(Debug, Clone)]
pub struct AstStruct {
    pos: SrcPos,
    members: Vec<AstStructMember>,
}


#[derive(Debug, Clone)]
struct AstStructMember {
    pos: SrcPos,
    name: String,
    ty: AstType,
}

#[derive(Debug, Clone)]
pub struct AstEnum {
    pub pos: SrcPos,
    pub variants: Vec<AstEnumVariant>,
}


#[derive(Debug, Clone)]
pub struct AstEnumVariant {
    pub pos: SrcPos,
    pub name: String,
    members: Vec<AstStructMember>,
}


impl Parsable for AstEnum {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Enum)), pos => pos
            expected "enum definition, starting with keyword `enum`")?;

        // parse body
        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "enum definition body starting with `{`")?;
        let mut variants = Vec::new();
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    parser.next()?;
                    break;
                },
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                    continue;
                },
                Ok(_) => (),
                Err(_) => {
                    return Err(ParseError::LexError(parser.next().err().unwrap()));
                }
            }

            variants.push(AstEnumVariant::parse(parser)?);
            // check if there is a comma
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                },
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    parser.next()?;
                    break;
                },
                Ok(_) => return Err(ParseError::UnexpectedToken(
                    Box::new(parser.next().unwrap()),
                    "expected `,` or `}` in enum definition".to_owned(),
                )),
                Err(_) => return Err(ParseError::LexError(
                    parser.next().err().unwrap()
                )),
            }
        }

        Ok(AstEnum {
            pos,
            variants,
        })
    }
}

impl Parsable for AstEnumVariant {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let (name, pos) = expect_token!(parser; (Token::Ident(name)), pos => (name, pos)
            expected "enum variant")?;
        let members = AstStruct::parse_body(parser)?;
        Ok(AstEnumVariant { pos, name, members })
    }
}


impl AstStruct {
    fn parse_named_body(parser: &mut Parser) -> Result<Vec<AstStructMember>, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "struct definition body starting with `{`")?;
        let mut members = Vec::new();
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    parser.next()?;
                    break;
                },
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                    continue;
                }
                Ok(_) => (),
                Err(_) => {
                    return Err(ParseError::LexError(parser.next().err().unwrap()));
                }
            }

            let member = AstStructMember::parse(parser)?;
            members.push(member);
            // check if there is a comma
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                },
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    parser.next()?;
                    break;
                }
                Ok(_) => return Err(ParseError::UnexpectedToken(
                    Box::new(parser.next().unwrap()),
                    "expected `,` or `}` in struct definition".to_owned()
                )),
                Err(_) => return Err(ParseError::LexError(
                    parser.next().err().unwrap()
                )),
            }
        }
        Ok(members)
    }

    fn parse_list_body(parser: &mut Parser) -> Result<Vec<AstStructMember>, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::BracketOpen))
            expected "struct definition body starting with `(`")?;
        let mut members = Vec::new();
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BracketClose))) => {
                    parser.next()?;
                    break;
                },
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                    continue;
                }
                Ok(_) => (),
                Err(_) => {
                    return Err(ParseError::LexError(parser.next().err().unwrap()));
                }
            }

            // parse member with enumerated name
            let pos = parser.pos().clone();
            let ty = AstType::parse(parser)?;
            let member = AstStructMember {
                pos,
                name: format!("{}", members.len()),
                ty,
            };
            members.push(member);

            // check if there is a comma
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                },
                Ok(local!(Token::Punct(Punct::BracketClose))) => {
                    parser.next()?;
                    break;
                }
                Ok(_) => return Err(ParseError::UnexpectedToken(
                    Box::new(parser.next().unwrap()),
                    "expected `,` or `)` in struct definition".to_owned()
                )),
                Err(_) => return Err(ParseError::LexError(
                    parser.next().err().unwrap()
                )),
            }
        }
        Ok(members)
    }

    fn parse_body(parser: &mut Parser) -> Result<Vec<AstStructMember>, ParseError> {
        match parser.peak() {
            Ok(local!(Token::Punct(Punct::BraceOpen))) => Self::parse_named_body(parser),
            Ok(local!(Token::Punct(Punct::BracketOpen))) => Self::parse_list_body(parser),
            Ok(_) => Ok(Vec::new()),
            Err(_) => Err(ParseError::LexError(parser.next().err().unwrap())),
        }
    }
}


impl Parsable for AstStruct {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Struct)), pos => pos
            expected "struct definition, starting with keyword `struct`")?;
        // let name = expect_token!(parser; (Token::Ident(name)) => name
        //     expected "struct name identifier")?;
        //
        // let mut env = ParameterEnv::parse(parser)?;
        // if let Ok(local!(Token::Key(KeyWord::Where))) = parser.peak() {
        //     env.parse_where(parser)?;
        // }

        // parse body
        let members = Self::parse_body(parser)?;

        Ok(AstStruct {
            pos,
            members,
        })
    }
}



impl Parsable for AstStructMember {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let (name, pos) = expect_token!(parser; (Token::Ident(name)), pos => (name, pos)
            expected "struct member name identifier")?;
        expect_token!(parser; (Token::Punct(Punct::Colon))
            expected "struct member type identifier starting with `:`")?;
        let ty = AstType::parse(parser)?;

        Ok(AstStructMember {
            name, ty, pos,
        })
    }
}



impl Display for AstEnum {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "enum ")?;
        write!(f, "{{")?;

        if !self.variants.is_empty() {
            writeln!(f)?;
            for variant in self.variants.iter() {
                writeln!(f, "{},", variant)?;
            }
        }
        write!(f, "}}")
    }
}

impl Display for AstEnumVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{{", self.name)?;

        if !self.members.is_empty() {
            writeln!(f)?;
            for members in self.members.iter() {
                writeln!(f, "{},", members)?;
            }
        }

        write!(f, "}}")
    }
}

impl Display for AstStruct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct ")?;
        write!(f, "{{")?;

        if !self.members.is_empty() {
            writeln!(f)?;
            for member in self.members.iter() {
                writeln!(f, "{},", member)?;
            }
        }
        write!(f, "}}")
    }
}

impl Display for AstStructMember {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.ty)
    }
}



#[cfg(test)]
mod test {
    use crate::parser::ast::ast_struct::{AstEnum, AstStruct};
    use crate::parser::ast::Parsable;
    use crate::parser::Parser;

    #[test]
    fn test_struct() {
        let src = r#"
        struct Human {
            name: String,
            age: u32,,
            networth: f64,
        }
        "#;
        let mut parser = Parser::new(src);
        let s = AstStruct::parse(&mut parser);

        assert!(s.is_ok());
        println!("{}", s.unwrap());
    }

    #[test]
    fn test_enum() {
        let src = r#"
        enum Option {
            None {},,
            Some { data: f64 }
        }
        "#;

        let mut parser = Parser::new(src);
        let s = AstEnum::parse(&mut parser);

        assert!(s.is_ok());
        println!("{}", s.unwrap());
    }
}
