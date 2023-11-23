use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, SrcPos, Token};
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, ParseError, Parser};
use crate::parser::ast::ast_type::AstType;
use crate::parser::r#type::core::CORE_VOID;

#[derive(Clone, Debug, PartialEq)]
pub struct AstContinue {
    pos: SrcPos,
}


impl Parsable for AstContinue {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Continue)), pos => pos
            expected "continue keyword")?;
        Ok(AstContinue {
            pos,
        }.into())
    }
}

impl From<AstContinue> for AstExpression {
    fn from(value: AstContinue) -> Self {
        AstExpression::Continue(value)
    }
}

impl AstTyped for AstContinue {
    fn ast_type(&self) -> Option<AstType> {
        Some(AstType::Base(CORE_VOID.into()))
    }
}

impl Display for AstContinue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "continue")
    }
}

