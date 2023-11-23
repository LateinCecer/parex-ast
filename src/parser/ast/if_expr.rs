use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Token};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::block::AstBlock;
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::r#type::core::CORE_VOID;


#[derive(Debug, Clone, PartialEq)]
enum IfCase {
    If(Box<AstExpression>, Box<AstBlock>, Option<Box<IfCase>>),
    Else(Box<AstBlock>)
}



#[derive(Debug, Clone, PartialEq)]
pub struct AstIf {
    case: IfCase,
}

impl Parsable for AstIf {
    type Output = AstIf;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        Ok(AstIf {
            case: IfCase::parse(parser)?
        })
    }
}

impl Parsable for IfCase {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::If)) expected "`if` expression")?;
        // parse condition and block
        let cond = Box::new(AstExpression::parse(parser)?);
        let block = Box::new(AstBlock::parse(parser)?);

        // look for else condition
        let else_cond = if let Ok(local!(Token::Key(KeyWord::Else))) = parser.peak() {
            parser.next()?;
            if let Ok(local!(Token::Key(KeyWord::If))) = parser.peak() {
                Some(Box::new(Self::parse(parser)?))
            } else {
                Some(Box::new(Self::Else(Box::new(AstBlock::parse(parser)?))))
            }
        } else {
            None
        };

        Ok(Self::If(cond, block, else_cond))
    }
}

impl From<AstIf> for AstExpression {
    fn from(value: AstIf) -> Self {
        AstExpression::If(value)
    }
}

impl AstTyped for AstIf {
    fn ast_type(&self) -> Option<AstType> {
        match &self.case {
            IfCase::If(_, block, Some(_)) => {
                block.ast_type()
            },
            IfCase::If(_, _, None) => {
                Some(AstType::Base(CORE_VOID.into()))
            },
            IfCase::Else(block) => {
                block.ast_type()
            },
        }
    }
}

impl Display for IfCase {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IfCase::If(cond, block, el) => {
                write!(f, "if {cond} {block}")?;
                if let Some(el) = el {
                    write!(f, " else {el}")?;
                }
                Ok(())
            }
            IfCase::Else(block) => write!(f, "{block}"),
        }
    }
}

impl Display for AstIf {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.case)
    }
}
