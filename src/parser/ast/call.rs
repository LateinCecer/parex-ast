use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::lexer::{Punct, Token};
use crate::parser::ast::expression::AstExpression;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::name::AstNameExpr;


#[derive(Clone, Debug, PartialEq)]
enum Params {
    Named(HashMap<String, AstExpression>),
    Located(Vec<AstExpression>),
}

impl Display for Params {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Params::Named(map) => Params::fmt_named(map, f),
            Params::Located(list) => Params::fmt_located(list, f),
        }
    }
}

impl Params {
    fn fmt_named(map: &HashMap<String, AstExpression>, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{ ")?;
        let mut iter = map.iter();
        if let Some((key, e)) = iter.next() {
            write!(f, "{key}: {e}")?;
        }
        for (key, e) in iter {
            write!(f, " {key}: {e}")?;
        }
        write!(f, " }}")?;
        Ok(())
    }

    fn fmt_located(list: &Vec<AstExpression>, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        let mut iter = list.iter();
        if let Some(e) = iter.next() {
            write!(f, "{e}")?;
        }
        for e in iter {
            write!(f, ", {e}")?;
        }
        write!(f, ")")?;
        Ok(())
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct AstInit {
    ty: AstType,
    params: Params,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstCall {
    func: Box<AstExpression>,
    params: Vec<AstExpression>,
}


#[derive(Clone, Debug, PartialEq)]
pub struct AstMethodCall {
    val: Box<AstExpression>,
    name: String,
    params: Vec<AstExpression>,
}

impl AstMethodCall {
    pub fn parse_lhs(
        parser: &mut Parser, val: Box<AstExpression>, name: String,
    ) -> Result<AstExpression, ParseError> {
        let params = AstInit::parse_ord_dependent(parser)?;
        Ok(AstMethodCall {
            val,
            name,
            params
        }.into())
    }
}

impl From<AstMethodCall> for AstExpression {
    fn from(value: AstMethodCall) -> Self {
        AstExpression::MethodCall(value)
    }
}


impl AstInit {
    fn parse_ord_dependent_unchecked(parser: &mut Parser) -> Result<Vec<AstExpression>, ParseError> {
        // parse with order-dependent parameters
        let mut params = Vec::<AstExpression>::new();
        loop {
            // check for closing bracket
            if let Ok(local!(Token::Punct(Punct::BracketClose))) = parser.peak() {
                parser.next()?;
                break;
            }
            let val = AstExpression::parse(parser)?;
            params.push(val);

            // check if next entry is available
            match parser.next() {
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    Ok(())
                },
                Ok(local!(Token::Punct(Punct::BracketClose))) => {
                    break;
                },
                Ok(t) => Err(ParseError::UnexpectedToken(
                    Box::new(t),
                    "comma `,` or closing bracket `)`".to_owned(),
                )),
                Err(e) => Err(ParseError::LexError(e)),
            }?;
        }
        Ok(params)
    }

    fn parse_ord_dependent(parser: &mut Parser) -> Result<Vec<AstExpression>, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::BracketOpen))
            expected "open bracket `(` leading order-dependent parameter list")?;
        Self::parse_ord_dependent_unchecked(parser)
    }

    pub fn parse_lhs(
        parser: &mut Parser, ty: AstType,
    ) -> Result<AstExpression, ParseError> {
        match parser.next() {
            // parse named parameter list
            Ok(local!(Token::Punct(Punct::BraceOpen))) => {
                // parse with named parameters
                let mut params = HashMap::<String, AstExpression>::new();
                loop {
                    // check for closing bracket
                    if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                        parser.next()?;
                        break;
                    }
                    let name = expect_token!(parser; (Token::Ident(name)) => name
                        expected "parameter name")?;

                    // check for value expression
                    if let Ok(local!(Token::Punct(Punct::Colon))) = parser.peak() {
                        parser.next()?;
                        let val = AstExpression::parse(parser)?;
                        params.insert(name, val);
                    } else {
                        let val = AstNameExpr::var(name.clone());
                        params.insert(name, val);
                    }

                    // check if next entry is available
                    match parser.next() {
                        Ok(local!(Token::Punct(Punct::Comma))) => {
                            Ok(())
                        },
                        Ok(local!(Token::Punct(Punct::BraceClose))) => {
                            break;
                        },
                        Ok(t) => Err(ParseError::UnexpectedToken(
                            Box::new(t),
                            "comma `,` or closing brace `}`"
                                .to_owned())),
                        Err(e) => Err(ParseError::LexError(e)),
                    }?;
                }

                Ok(AstInit {
                    ty,
                    params: Params::Named(params),
                }.into())
            },
            // parse order-dependent parameter list
            Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                // parse with order-dependent parameters
                let params = Self::parse_ord_dependent_unchecked(parser)?;
                Ok(AstInit {
                    ty,
                    params: Params::Located(params),
                }.into())
            },
            Ok(e) => Err(ParseError::UnexpectedToken(
                Box::new(e),
                "call operator or struct init leading with `(` or `{`".to_owned())),
            Err(e) => Err(ParseError::LexError(e)),
        }
    }
}

impl AstCall {
    pub fn parse_lhs(
        parser: &mut Parser, func: Box<AstExpression>
    ) -> Result<AstExpression, ParseError> {
        match parser.next() {
            // parse order-dependent parameter list
            Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                // parse with order-dependent parameters
                let params = AstInit::parse_ord_dependent_unchecked(parser)?;
                Ok(AstCall {
                    func,
                    params,
                }.into())
            },
            Ok(e) => Err(ParseError::UnexpectedToken(
                Box::new(e),
                "call operator or struct init leading with `(` or `{`".to_owned())),
            Err(e) => Err(ParseError::LexError(e)),
        }
    }
}

impl From<AstCall> for AstExpression {
    fn from(value: AstCall) -> Self {
        AstExpression::FunctionCall(value)
    }
}

impl Parsable for AstCall {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let lhs = Box::new(AstExpression::parse(parser)?);
        AstCall::parse_lhs(parser, lhs)
    }
}

impl Parsable for AstInit {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let lhs = AstType::parse(parser)?;
        AstInit::parse_lhs(parser, lhs)
    }
}

impl From<AstInit> for  AstExpression {
    fn from(value: AstInit) -> Self {
        AstExpression::Init(value)
    }
}

impl Display for AstMethodCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}).{}", self.val, self.name)?;
        Params::fmt_located(&self.params, f)
    }
}

impl Display for AstCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.func)?;
        Params::fmt_located(&self.params, f)
    }
}

impl Display for AstInit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.ty, self.params)
    }
}
