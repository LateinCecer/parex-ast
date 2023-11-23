use std::fmt::{Display, Formatter};
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::ast::expression::AstExpression;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    Ref,
    Deref,
    Unary,
    Invert,
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Ref => write!(f, "*const"),
            PrefixOp::Deref => write!(f, "*mut"),
            PrefixOp::Unary => write!(f, "-"),
            PrefixOp::Invert => write!(f, "!"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstPrefixExpr {
    pub val: Box<AstExpression>,
    pub op: PrefixOp,
    pub pos: SrcPos,
}

impl AstPrefixExpr {
    pub fn new(val: Box<AstExpression>, op: PrefixOp, pos: SrcPos) -> Self {
        AstPrefixExpr {
            val, op, pos
        }
    }
}

impl Parsable for AstPrefixExpr {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let (op, pos) = expect_token!(parser;
            (Token::Punct(Punct::And)), pos => (PrefixOp::Ref, pos);
            (Token::Punct(Punct::Astrix)), pos => (PrefixOp::Deref, pos);
            (Token::Punct(Punct::Minus)), pos => (PrefixOp::Unary, pos);
            (Token::Punct(Punct::Not)), pos => (PrefixOp::Invert, pos)
        expected "prefix operator `*`, `&`, `-`, or `!`")?;

        let val = AstExpression::parse_primary(parser)?;
        Ok(AstPrefixExpr {
            val: Box::new(val),
            op,
            pos
        }.into())
    }
}

impl From<AstPrefixExpr> for AstExpression {
    fn from(value: AstPrefixExpr) -> Self {
        AstExpression::Prefix(value)
    }
}

impl Display for AstPrefixExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.op, self.val)
    }
}


#[cfg(test)]
mod test {
    use crate::parser::ast::expression::AstExpression;
    use crate::parser::ast::literal::AstNumberLiteral;
    use crate::parser::ast::Parsable;
    use crate::parser::ast::prefix_operator::{AstPrefixExpr, PrefixOp};
    use crate::parser::Parser;

    #[test]
    fn test_prefix() {
        assert_eq!(
            AstExpression::parse(&mut Parser::new("&42")),
            Ok(AstExpression::Prefix(AstPrefixExpr {
                val: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(42, None))),
                op: PrefixOp::Ref,
                pos: Default::default(),
            }))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::new("*42")),
            Ok(AstExpression::Prefix(AstPrefixExpr {
                val: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(42, None))),
                op: PrefixOp::Deref,
                pos: Default::default(),
            }))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::new("-42")),
            Ok(AstExpression::Prefix(AstPrefixExpr {
                val: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(42, None))),
                op: PrefixOp::Unary,
                pos: Default::default(),
            }))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::new("!42")),
            Ok(AstExpression::Prefix(AstPrefixExpr {
                val: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(42, None))),
                op: PrefixOp::Invert,
                pos: Default::default(),
            }))
        );
    }
}
