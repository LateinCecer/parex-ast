use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::ast_struct::{AstEnum, AstStruct};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::Parsable;
use crate::parser::parameter_env::ParameterEnv;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::resolver::ItemVariant;


#[derive(Debug, Clone)]
pub enum AstTypeVariant {
    Struct(AstStruct),
    Enum(AstEnum),
    Alias(AstType),
}

#[derive(Debug, Clone)]
pub struct AstTypeDef {
    name: String,
    pos: SrcPos,
    env: ParameterEnv,
    var: AstTypeVariant,
}

impl AstTypeVariant {
    pub fn push_to_env(&self, parser: &mut Parser, name: String, pos: SrcPos) -> Result<(), ParseError> {
        match &self {
            AstTypeVariant::Struct(s) => {
                parser.env.push_top_level_item(name, pos, ItemVariant::Type)?;
            }
            AstTypeVariant::Enum(e) => {
                let scope = parser.env.push_top_level_item(
                    name, pos, ItemVariant::Type,
                )?;
                parser.env.revert_to_scope(&scope);
                for var in e.variants.iter() {
                    parser.env.push_top_level_item(
                        var.name.clone(),
                        var.pos,
                        ItemVariant::Type,
                    )?;
                }
                parser.env.pop();
            }
            AstTypeVariant::Alias(a) => {
                parser.env.push_top_level_item(name, pos, ItemVariant::Type)?;
            }
        }
        Ok(())
    }
}

impl AstTypeDef {
    pub fn push_to_env(&self, parser: &mut Parser) -> Result<(), ParseError> {
        self.var.push_to_env(parser, self.name.clone(), self.pos)
    }
}

impl Parsable for AstTypeVariant {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        match parser.peak() {
            Ok(local!(Token::Key(KeyWord::Struct))) => {
                Ok(Self::Struct(AstStruct::parse(parser)?))
            },
            Ok(local!(Token::Key(KeyWord::Enum))) => {
                Ok(Self::Enum(AstEnum::parse(parser)?))
            },
            Ok(_) => {
                let tmp = Self::Alias(AstType::parse(parser)?);
                expect_token!(parser; (Token::Punct(Punct::Semicolon)) expected "`;`")?;
                Ok(tmp)
            },
            Err(_) => {
                Err(parser.next().err().unwrap().into())
            }
        }
    }
}

impl Parsable for AstTypeDef {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Type)), pos => pos
            expected "type definition starting with `type` keyword")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "type name identifier")?;

        let mut env = ParameterEnv::parse(parser)?;
        if let Ok(local!(Token::Key(KeyWord::Where))) = parser.peak() {
            env.parse_where(parser)?;
        }

        // parse definition
        expect_token!(parser; (Token::Punct(Punct::Assign))
            expected "type definition starting with `=`")?;
        let var = AstTypeVariant::parse(parser)?;

        Ok(AstTypeDef {
            name,
            pos,
            env,
            var,
        })
    }
}

impl Display for AstTypeVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstTypeVariant::Struct(s) => {
                write!(f, "{s}")
            }
            AstTypeVariant::Enum(e) => {
                write!(f, "{e}")
            }
            AstTypeVariant::Alias(a) => {
                write!(f, "{a}")
            }
        }
    }
}

impl Display for AstTypeDef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "type {}{} ", self.name, self.env)?;
        self.env.fmt_where(f)?;
        write!(f, "= {}", self.var)
    }
}
