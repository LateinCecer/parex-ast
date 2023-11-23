use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, Token};
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_type::AstType;
use crate::parser::r#type::core::CORE_VOID;


#[derive(Clone, Debug, PartialEq)]
struct SwitchCase {
    cond: AstExpression,
    val: AstExpression,
}

impl Display for SwitchCase {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.cond, self.val)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstSwitch {
    cond: Box<AstExpression>,
    cases: Vec<SwitchCase>,
}

impl SwitchCase {
    fn must_be_terminated(&self) -> bool {
        self.val.must_terminate_statement()
    }
}

impl Parsable for SwitchCase {
    type Output = SwitchCase;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let cond = AstExpression::parse(parser)?;
        expect_token!(parser; (Token::Punct(Punct::BigRightArrow))
            expected "switch case value starting with `=>`")?;
        let val = AstExpression::parse(parser)?;

        Ok(SwitchCase {
            cond,
            val,
        })
    }
}


impl Parsable for AstSwitch {
    type Output = AstSwitch;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::Switch))
            expected "switch case")?;
        let cond = Box::new(AstExpression::parse(parser)?);

        expect_token!(parser; (Token::Punct(Punct::BraceOpen)) expected "`{`")?;
        let mut cases = Vec::new();
        let mut must_terminate = false;
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BraceClose))) => {
                    parser.next()?;
                    break;
                },
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                    must_terminate = false;
                    continue;
                },
                _ => ()
            }

            // check for termination
            if must_terminate {
                expect_token!(parser; (Token::Punct(Punct::Comma)) expected "switch case termination `,` or `}`")?;
            }
            // get case
            let case = SwitchCase::parse(parser)?;
            must_terminate = case.must_be_terminated();
            cases.push(case);
        }

        Ok(AstSwitch {
            cond,
            cases
        })
    }
}

impl From<AstSwitch> for AstExpression {
    fn from(value: AstSwitch) -> Self {
        AstExpression::Switch(value)
    }
}

impl AstTyped for SwitchCase {
    fn ast_type(&self) -> Option<AstType> {
        self.val.ast_type()
    }
}

impl AstTyped for AstSwitch {
    fn ast_type(&self) -> Option<AstType> {
        if let Some(last) = self.cases.first() {
            last.ast_type()
        } else {
            Some(AstType::Base(CORE_VOID.into()))
        }
    }
}

impl Display for AstSwitch {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "switch {{ ")?;

        let mut iter = self.cases.iter();
        if let Some(e) = iter.next() {
            write!(f, "{e}")?;
        }
        for e in iter {
            writeln!(f, ",")?;
            write!(f, "{e}")?;
        }

        write!(f, " }}")
    }
}

