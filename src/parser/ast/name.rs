use std::fmt::{Display, Formatter};
use std::ops::Deref;
use crate::parser::ast::Parsable;
use crate::parser::{ParseError, Parser};
use crate::parser::ast::ast_type::{AstType, TypeName};
use crate::parser::ast::expression::AstExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct AstNameExpr {
    names: TypeName,
}

impl AstNameExpr {
    pub fn var(name: String) -> AstExpression {
        AstNameExpr {
            names: name.into(),
        }.into()
    }
}

impl From<AstNameExpr> for AstType {
    fn from(value: AstNameExpr) -> Self {
        AstType::Base(value.names)
    }
}

impl Deref for AstNameExpr {
    type Target = TypeName;

    fn deref(&self) -> &Self::Target {
        &self.names
    }
}

impl Parsable for AstNameExpr {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        Ok(AstNameExpr {
            names: TypeName::parse(parser)?
        })
    }
}

impl From<AstNameExpr> for AstExpression {
    fn from(value: AstNameExpr) -> Self {
        AstExpression::Name(value)
    }
}

impl Display for AstNameExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.names.iter();
        if let Some(e) = iter.next() {
            write!(f, "{e}")?;
        }
        loop {
            if let Some(e) = iter.next() {
                write!(f, "::{e}")?;
            } else {
                break;
            }
        }
        Ok(())
    }
}
