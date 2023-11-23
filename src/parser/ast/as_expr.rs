use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::{expect_token, ParseError, Parser};
use crate::parser::ast::Parsable;

#[derive(Clone, Debug, PartialEq)]
pub struct AsExpr {
    lhs: Box<AstExpression>,
    target: AstType,
    pos: SrcPos,
}

impl AsExpr {
    pub fn parse_lhs(parser: &mut Parser, lhs: Box<AstExpression>) -> Result<Self, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::As)), pos => pos
            expected "as expression")?;
        let target = AstType::parse(parser)?;
        Ok(AsExpr {
            lhs,
            pos,
            target,
        })
    }
}

impl From<AsExpr> for AstExpression {
    fn from(value: AsExpr) -> Self {
        AstExpression::As(value)
    }
}

impl Parsable for AsExpr {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let lhs = AstExpression::parse(parser)?;
        AsExpr::parse_lhs(parser, Box::new(lhs))
    }
}

impl AstTyped for AsExpr {
    fn ast_type(&self) -> Option<AstType> {
        Some(self.target.clone())
    }
}

impl Display for AsExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} as {}", self.lhs, self.target)
    }
}
