use std::fmt::{Display, Formatter};
use crate::lexer::{Punct, Token};
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_type::AstType;
use crate::parser::r#type::core::CORE_VOID;


#[derive(Debug, Clone, PartialEq)]
pub struct AstBlock {
    content: Vec<AstExpression>,
    last_terminated: bool,
}

impl AstBlock {
    pub fn find_break_value(&self) -> Option<AstType> {
        for entry in self.content.iter() {
            match entry {
                AstExpression::Break(b) => return b.break_value(),
                AstExpression::Block(b) => return b.find_break_value(),
                _ => (),
            }
        }
        None
    }
}

impl Parsable for AstBlock {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "block started by `{`")?;

        let mut content = Vec::new();
        let mut need_termination = false;
        let mut last_terminated = false;
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    parser.next()?;
                    break;
                },
                Ok(local!(Token::Punct(Punct::Semicolon))) => {
                    parser.next()?;
                    need_termination = false;
                    last_terminated = true;
                    continue;
                }
                _ => ()
            }

            if need_termination {
                // this expect will always fail, but the emitted error can either be an
                // `UnexpectedToken` or an `UnexpectedEndOfStream`. Using the macro is the easiest
                // way to handle this
                expect_token!(parser; (Token::Punct(Punct::Semicolon)) expected "`;`")?;
            }

            // parse expression
            let expr = AstExpression::parse(parser)?;
            need_termination = expr.must_terminate_statement();
            last_terminated = false;
            content.push(expr);
        }

        Ok(AstBlock {
            content,
            last_terminated,
        })
    }
}

impl From<AstBlock> for AstExpression {
    fn from(value: AstBlock) -> Self {
        AstExpression::Block(value)
    }
}

impl AstTyped for AstBlock {
    fn ast_type(&self) -> Option<AstType> {
        if self.last_terminated {
            if let Some(last) = self.content.last() {
                last.ast_type()
            } else {
                Some(AstType::Base(CORE_VOID.into()))
            }
        } else {
            Some(AstType::Base(CORE_VOID.into()))
        }
    }
}

impl Display for AstBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;

        let len = self.content.len();
        for (i, e) in self.content.iter().enumerate() {
            write!(f, "{e}")?;

            // check if end if reached
            if i == len - 1 {
                if self.last_terminated {
                    write!(f, ";")?;
                }
            } else if e.must_terminate_statement() {
                write!(f, ";")?;
            }
            writeln!(f)?;
        }

        write!(f, "}}")
    }
}
