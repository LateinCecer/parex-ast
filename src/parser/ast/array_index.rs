use std::fmt::{Display, Formatter};
use crate::lexer::{Punct, Token};
use crate::parser::ast::expression::AstExpression;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, ParseError, Parser};


#[derive(Clone, Debug, PartialEq)]
pub struct AstArrayIndex {
    lhs: Box<AstExpression>,
    idx: Box<AstExpression>,
}

impl AstArrayIndex {
    pub fn parse_lhs(parser: &mut Parser, lhs: Box<AstExpression>) -> Result<AstExpression, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::SquareOpen)) expected "index operator `[`")?;
        let idx = Box::new(AstExpression::parse(parser)?);
        expect_token!(parser; (Token::Punct(Punct::SquareClose)) expected "index operator `]`")?;

        Ok(AstArrayIndex {
            lhs,
            idx
        }.into())
    }
}

impl From<AstArrayIndex> for AstExpression {
    fn from(value: AstArrayIndex) -> Self {
        AstExpression::ArrayIndex(value)
    }
}

impl Parsable for AstArrayIndex {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let rhs = Box::new(AstExpression::parse(parser)?);
        AstArrayIndex::parse_lhs(parser, rhs)
    }
}

impl Display for AstArrayIndex {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[{}]", self.lhs, self.idx)
    }
}
