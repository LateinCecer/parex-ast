use std::fmt::{Display, Formatter};
use crate::lexer::{Punct, SrcPos, Token};
use crate::parser::ast::expression::AstExpression;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser, ParseResult};



#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,

    And,
    Or,
    Xor,

    LAnd,
    LOr,
    LXor,

    Eq,
    Neq,
    Gt,
    Lt,
    Geq,
    Leq,

    /// `<<`
    Lst,
    /// `>>`
    Rst,
    Rem,

    Assign,
}

impl BinaryOp {
    fn punct(&self) -> Punct {
        match self {
            BinaryOp::Add => Punct::Plus,
            BinaryOp::Sub => Punct::Minus,
            BinaryOp::Mul => Punct::Astrix,
            BinaryOp::Div => Punct::Slash,
            BinaryOp::And => Punct::And,
            BinaryOp::Or => Punct::Or,
            BinaryOp::Xor => Punct::Xor,
            BinaryOp::LAnd => Punct::LAnd,
            BinaryOp::LOr => Punct::LOr,
            BinaryOp::LXor => Punct::LXor,
            BinaryOp::Eq => Punct::Eq,
            BinaryOp::Neq => Punct::Neq,
            BinaryOp::Gt => Punct::Gt,
            BinaryOp::Lt => Punct::Lt,
            BinaryOp::Geq => Punct::Geq,
            BinaryOp::Leq => Punct::Leq,
            BinaryOp::Lst => Punct::Lst,
            BinaryOp::Rst => Punct::Rst,
            BinaryOp::Rem => Punct::Rem,

            BinaryOp::Assign => Punct::Assign,
        }
    }
}

impl TryFrom<Punct> for BinaryOp {
    type Error = ();

    fn try_from(value: Punct) -> Result<Self, ()> {
        match value {
            Punct::Plus => Ok(Self::Add),
            Punct::Minus => Ok(Self::Sub),
            Punct::Astrix => Ok(Self::Mul),
            Punct::Slash => Ok(Self::Div),
            Punct::Rem => Ok(Self::Rem),
            Punct::And => Ok(Self::And),
            Punct::Or => Ok(Self::Or),
            Punct::Xor => Ok(Self::Xor),
            Punct::LAnd => Ok(Self::LAnd),
            Punct::LOr => Ok(Self::LOr),
            Punct::LXor => Ok(Self::LXor),
            Punct::Eq => Ok(Self::Eq),
            Punct::Neq => Ok(Self::Neq),
            Punct::Lt => Ok(Self::Lt),
            Punct::Gt => Ok(Self::Gt),
            Punct::Leq => Ok(Self::Leq),
            Punct::Geq => Ok(Self::Geq),
            Punct::Lst => Ok(Self::Lst),
            Punct::Rst => Ok(Self::Rst),
            Punct::Assign => Ok(Self::Assign),
            _ => Err(()),
        }
    }
}



#[derive(Clone, PartialEq, Debug)]
pub struct AstBinaryExpr {
    pub op: BinaryOp,
    pub lhs: Box<AstExpression>,
    pub rhs: Box<AstExpression>,
    pub pos: SrcPos,
}

impl Parsable for AstBinaryExpr {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let lhs = AstExpression::parse_primary(parser)?;
        AstBinaryExpr::parse_operator(parser, 0, lhs)
    }
}

impl AstBinaryExpr {
    fn parse_operator(
        parser: &mut Parser, expr_prec: usize, mut lhs: AstExpression
    ) -> ParseResult<AstExpression> {
        // if this is a binary operation, find its precedence
        loop {
            let precedence = if let Ok(local!(Token::Punct(punct))) = parser.peak() {
                punct.precedence()
            } else {
                None
            };

            // if this is a binop that binds at least as tightly as the current binop, consume it,
            // otherwise we are done.
            if precedence.is_none() || precedence.unwrap() < expr_prec {
                break Ok(lhs);
            }
            let tok_prec = precedence.unwrap();

            // okay, we know this is a binop
            let (punct, pos) = expect_token!(parser;
                (Token::Punct(p)), pos => (p, pos)
                expected "to find punctuation token for binary operations")?;
            // parse the primary expression after the binop operator
            let mut rhs = AstExpression::parse_primary(parser)?;

            // if binop binds less tightly with rhs then the operator after rhs, let the pending
            // operator take rhs and its lhs
            let next_prec = if let Ok(local!(Token::Punct(p))) = parser.peak() {
                p.precedence()
            } else {
                None
            };

            match next_prec {
                Some(next_prec) if tok_prec < next_prec => {
                    rhs = Self::parse_operator(parser, tok_prec + 1, rhs)?;
                }
                _ => (),
            }
            // join left and right side of the binop
            lhs = AstBinaryExpr {
                op: punct.try_into().map_err(|_| ParseError::UnexpectedToken(
                    Box::new(Token::Punct(punct).localize(pos)),
                    "to find punctuation token for binary operations".to_owned()))?,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                pos,
            }.into();
        }
    }
}

impl From<AstBinaryExpr> for AstExpression {
    fn from(value: AstBinaryExpr) -> Self {
        AstExpression::Binary(value)
    }
}

impl Display for AstBinaryExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op.punct(), self.rhs)
    }
}


#[cfg(test)]
mod test {
    use crate::lexer::SrcPos;
    use crate::parser::ast::binary::{AstBinaryExpr, BinaryOp};
    use crate::parser::ast::expression::AstExpression;
    use crate::parser::ast::literal::AstNumberLiteral;
    use crate::parser::ast::Parsable;
    use crate::parser::Parser;

    #[test]
    fn test_simple() {
        for op in [
            BinaryOp::Add,
            BinaryOp::Sub,
            BinaryOp::Mul,
            BinaryOp::Div,
            BinaryOp::Rem,
            BinaryOp::And,
            BinaryOp::Or,
            BinaryOp::Xor,
            BinaryOp::LAnd,
            BinaryOp::LOr,
            BinaryOp::LXor,
            BinaryOp::Eq,
            BinaryOp::Neq,
            BinaryOp::Gt,
            BinaryOp::Lt,
            BinaryOp::Geq,
            BinaryOp::Leq,
            BinaryOp::Lst,
            BinaryOp::Rst,
            BinaryOp::Assign,
        ] {
            let src = format!("1 {} 2", op.punct());
            let mut parser = Parser::new(&src);
            let expr = AstExpression::parse(&mut parser);
            assert_eq!(expr, Ok(AstExpression::Binary(AstBinaryExpr {
                op,
                lhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(1, None))),
                rhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(2, None))),
                pos: SrcPos::new(0, 2),
            })))
        }
    }

    #[test]
    fn test_precedence_1() {
        let src = "1 + 2 * 3";
        let mut parser = Parser::new(src);
        let expr = AstExpression::parse(&mut parser);

        assert_eq!(expr, Ok(AstExpression::Binary(AstBinaryExpr {
            op: BinaryOp::Add,
            lhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(1, None))),
            rhs: Box::new(AstExpression::Binary(AstBinaryExpr {
                op: BinaryOp::Mul,
                lhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(2, None))),
                rhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(3, None))),
                pos: SrcPos::new(0, 6),
            })),
            pos: SrcPos::new(0, 2),
        })));
    }

    #[test]
    fn test_precedence_2() {
        let src = "2 * 3 + 1";
        let mut parser = Parser::new(src);
        let expr = AstExpression::parse(&mut parser);

        assert_eq!(expr, Ok(AstExpression::Binary(AstBinaryExpr {
            op: BinaryOp::Add,
            lhs: Box::new(AstExpression::Binary(AstBinaryExpr {
                op: BinaryOp::Mul,
                lhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(2, None))),
                rhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(3, None))),
                pos: SrcPos::new(0, 2),
            })),
            rhs: Box::new(AstExpression::NumberLit(AstNumberLiteral::Int(1, None))),
            pos: SrcPos::new(0, 6),
        })));
    }
}
