use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, Token};
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_type::AstType;
use crate::parser::r#type::core::CORE_VOID;


#[derive(Debug, Clone, PartialEq)]
pub struct AstReturn {
    val: Option<Box<AstExpression>>,
}

impl AstReturn {
    pub fn return_value(&self) -> Option<AstType> {
        if let Some(expr) = &self.val {
            expr.ast_type()
        } else {
            Some(AstType::Base(CORE_VOID.into()))
        }
    }
}

impl Parsable for AstReturn {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::Return))
            expected "return keyword")?;

        let val = if let Ok(local!(
            Token::Punct(Punct::Semicolon | Punct::Comma | Punct::BraceClose))) = parser.peak() {
            None
        } else {
            Some(Box::new(AstExpression::parse(parser)?))
        };

        Ok(AstReturn {
            val,
        }.into())
    }
}

impl From<AstReturn> for AstExpression {
    fn from(value: AstReturn) -> Self {
        AstExpression::Return(value)
    }
}

impl AstTyped for AstReturn {
    fn ast_type(&self) -> Option<AstType> {
        Some(AstType::Base(CORE_VOID.into()))
    }
}

impl Display for AstReturn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "return")?;
        if let Some(e) = &self.val {
            write!(f, " {e}")?;
        }
        Ok(())
    }
}
