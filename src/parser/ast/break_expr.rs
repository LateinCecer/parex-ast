use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, Token};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::r#type::core::CORE_VOID;

#[derive(Clone, Debug, PartialEq)]
pub struct AstBreak {
    pub val: Option<Box<AstExpression>>,
}

impl AstBreak {
    pub fn break_value(&self) -> Option<AstType> {
        if let Some(expr) = &self.val {
            expr.ast_type()
        } else {
            Some(AstType::Base(CORE_VOID.into()))
        }
    }
}

impl Parsable for AstBreak {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::Break)) expected "break keyword")?;

        let val = if let Ok(local!(
            Token::Punct(Punct::Semicolon | Punct::Comma | Punct::BraceClose))) = parser.peak() {
            None
        } else {
            Some(Box::new(AstExpression::parse(parser)?))
        };

        Ok(AstBreak {
            val,
        }.into())
    }
}

impl AstTyped for AstBreak {
    fn ast_type(&self) -> Option<AstType> {
        Some(AstType::Base(CORE_VOID.into()))
    }
}

impl From<AstBreak> for AstExpression {
    fn from(value: AstBreak) -> Self {
        AstExpression::Break(value)
    }
}

impl Display for AstBreak {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "break")?;
        if let Some(val) = &self.val {
            write!(f, " {val}")?;
        }
        Ok(())
    }
}

