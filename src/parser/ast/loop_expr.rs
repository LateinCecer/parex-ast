use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::block::AstBlock;
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, ParseError, Parser};

#[derive(Debug, Clone, PartialEq)]
pub struct AstLoop {
    pos: SrcPos,
    block: Box<AstBlock>,
}

impl Parsable for AstLoop {
    type Output = AstLoop;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Loop)), pos => pos
            expected "loop expression")?;
        let block = Box::new(AstBlock::parse(parser)?);

        Ok(AstLoop {
            pos,
            block,
        })
    }
}

impl From<AstLoop> for AstExpression {
    fn from(value: AstLoop) -> Self {
        AstExpression::Loop(value)
    }
}

impl AstTyped for AstLoop {
    fn ast_type(&self) -> Option<AstType> {
        self.block.find_break_value()
    }
}

impl Display for AstLoop {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "loop {}", self.block)
    }
}
