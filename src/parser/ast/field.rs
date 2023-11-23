use std::fmt::{Display, Formatter};
use crate::parser::ast::expression::AstExpression;


#[derive(Clone, Debug, PartialEq)]
pub struct AstFieldExpr {
    lhs: Box<AstExpression>,
    field: String,
}

impl AstFieldExpr {
    pub fn new(lhs: Box<AstExpression>, field: String) -> AstFieldExpr {
        AstFieldExpr {
            lhs, field
        }
    }
}

impl From<AstFieldExpr> for AstExpression {
    fn from(value: AstFieldExpr) -> Self {
        AstExpression::Field(value)
    }
}

impl Display for AstFieldExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.lhs, self.field)
    }
}


