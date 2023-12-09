use std::fmt::{Display, Formatter};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::expression::{AstExpression, AstTyped, AstTypedMut};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, ParseError, Parser};
use crate::parser::r#type::core::{CORE_CHAR, CORE_F32, CORE_F64, CORE_I16, CORE_I32, CORE_I64, CORE_I8, CORE_ISIZE, CORE_STR, CORE_U16, CORE_U32, CORE_U64, CORE_U8, CORE_USIZE, CoreType};
use crate::lexer::{NumWithLeading, Token};


#[derive(Debug, Clone, PartialEq)]
pub enum AstNumberLiteral {
    Int(usize, Option<AstType>),
    Float(f64, Option<AstType>),
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstBoolLiteral {
    value: bool,
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstCharLiteral {
    value: char,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstStringLiteral {
    value: String,
}

impl AstTyped for AstCharLiteral {
    fn ast_type(&self) -> Option<AstType> {
        Some(AstType::Base(CORE_CHAR.into()))
    }
}

impl AstTyped for AstStringLiteral {
    fn ast_type(&self) -> Option<AstType> {
        Some(AstType::Ptr(Box::new(AstType::Base(CORE_STR.into()))))
    }
}

impl AstTyped for AstNumberLiteral {
    fn ast_type(&self) -> Option<AstType> {
        match self {
            AstNumberLiteral::Int(_, ty) => ty.clone(),
            AstNumberLiteral::Float(_, ty) => ty.clone(),
        }
    }
}

impl AstTypedMut for AstNumberLiteral {
    fn suggest(&mut self, val: AstType) -> bool {
        match self {
            AstNumberLiteral::Int(_, Some(ty)) => *ty == val,
            AstNumberLiteral::Int(_, ty) if val.is_integer() => {
                *ty = Some(val);
                true
            },
            AstNumberLiteral::Float(_, Some(ty)) => *ty == val,
            AstNumberLiteral::Float(_, ty) => {
                *ty = Some(val);
                true
            }
            _ => false
        }
    }
}

impl Parsable for AstStringLiteral {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let value = expect_token!(parser; (Token::StrLiteral(s)) => s
            expected "string literal")?;
        Ok(AstStringLiteral {
            value,
        }.into())
    }
}

impl From<AstStringLiteral> for AstExpression {
    fn from(value: AstStringLiteral) -> Self {
        AstExpression::StrLit(value)
    }
}

impl Parsable for AstCharLiteral {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let value = expect_token!(parser;
            (Token::CharLiteral(c)) => c expected "char literal")?;
        Ok(AstCharLiteral {
            value,
        }.into())
    }
}

impl From<AstCharLiteral> for AstExpression {
    fn from(value: AstCharLiteral) -> Self {
        AstExpression::CharLit(value)
    }
}

impl Parsable for AstBoolLiteral {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let (value, pos) = expect_token!(parser; (Token::Ident(s)), pos => (s, pos) expected "boolean literal")?;
        if value == "true" {
            Ok(AstBoolLiteral {
                value: true,
            }.into())
        } else if value == "false" {
            Ok(AstBoolLiteral {
                value: false,
            }.into())
        } else {
            Err(ParseError::UnexpectedToken(Box::new(Token::Ident(value).localize(pos)), "boolean literal `true` or `false`".to_owned()))
        }
    }
}

impl From<AstBoolLiteral> for AstExpression {
    fn from(value: AstBoolLiteral) -> Self {
        AstExpression::BoolLit(value)
    }
}


impl Parsable for AstNumberLiteral {
    type Output = AstExpression;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let (x, y, z, hint, pos) = expect_token!(parser;
            (Token::NumLiteral(x, y, z, hint)), pos => (x, y, z, hint, pos) expected "number literal")?;
        let t = Token::NumLiteral(x, y.clone(), z, hint.clone())
            .localize(pos);

        match hint {
            Some(hint) if hint == CORE_U8 => {
                // try to parse as u16
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_U8.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<u8>::ast_type())).into())
            },
            Some(hint) if hint == CORE_U16 => {
                // try to parse as u16
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_U16.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<u16>::ast_type())).into())
            },
            Some(hint) if hint == CORE_U32 => {
                // try to parse as u32
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_U32.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<u32>::ast_type())).into())
            },
            Some(hint) if hint == CORE_U64 => {
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_U64.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<u64>::ast_type())).into())
            },
            Some(hint) if hint == CORE_I8 => {
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_I8.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<i8>::ast_type())).into())
            },
            Some(hint) if hint == CORE_I16 =>  {
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_I16.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<i16>::ast_type())).into())
            },
            Some(hint) if hint == CORE_I32 => {
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_I32.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<i32>::ast_type())).into())
            },
            Some(hint) if hint == CORE_I64 => {
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_I64.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<i64>::ast_type())).into())
            },
            Some(hint) if hint == CORE_F32 => {
                // parse as f32
                Ok(AstNumberLiteral::Float(convert_float(x, y, z), Some(CoreType::<f32>::ast_type())).into())
            },
            Some(hint) if hint == CORE_F64 => {
                // parse as f64
                Ok(AstNumberLiteral::Float(convert_float(x, y, z), Some(CoreType::<f64>::ast_type())).into())
            },
            Some(hint) if hint == CORE_USIZE => {
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_USIZE.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<usize>::ast_type())).into())
            },
            Some(hint) if hint == CORE_ISIZE => {
                if y.is_some() || z.is_some() {
                    return Err(ParseError::NumberConversionError(
                        Box::new(t), CORE_ISIZE.to_owned()
                    ));
                }
                Ok(AstNumberLiteral::Int(x, Some(CoreType::<isize>::ast_type())).into())
            },
            Some(_) => {
                Err(ParseError::NumberConversionError(Box::new(t), "`u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`, `usize`, `isize`, `f32`, `f64`".to_owned()))
            },
            None => {
                if y.is_some() || z.is_some() {
                    // parse as some float
                    Ok(AstNumberLiteral::Float(convert_float(x, y, z), None).into())
                } else {
                    Ok(AstNumberLiteral::Int(x, None).into())
                }
            },
        }
    }
}

fn convert_float(x: usize, y:  Option<NumWithLeading>, z: Option<isize>) -> f64 {
    let exp = z.unwrap_or(0) as i32;
    let mut val = x as f64 * f64::powi(10_f64, exp);
    if let Some(NumWithLeading(num, leading)) = y {
        val += (num as f64) * f64::powi(10_f64, -(leading as i32 + 1 + usize::ilog10(num) as i32) + exp);
    }
    val
}

impl From<AstNumberLiteral> for AstExpression {
    fn from(value: AstNumberLiteral) -> Self {
        AstExpression::NumberLit(value)
    }
}

impl Display for AstBoolLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Display for AstNumberLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstNumberLiteral::Int(val, ty) => {
                write!(f, "{val}")?;
                if let Some(ty) = ty {
                    write!(f, "_{ty}")?;
                }
                Ok(())
            }
            AstNumberLiteral::Float(val, ty) => {
                write!(f, "{val}")?;
                if let Some(ty) = ty {
                    write!(f, "_{ty}")?;
                }
                Ok(())
            }
        }
    }
}

impl Display for AstStringLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}

impl Display for AstCharLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "'{}'", self.value)
    }
}



#[cfg(test)]
mod test {
    use crate::parser::ast::ast_type::AstType;
    use crate::parser::ast::expression::AstExpression;
    use crate::parser::ast::literal::{AstBoolLiteral, AstCharLiteral, AstNumberLiteral, AstStringLiteral};
    use crate::parser::ast::Parsable;
    use crate::parser::Parser;
    use crate::parser::resolver::TopLevelNameResolver;

    #[test]
    fn test_string() {
        let mut env = TopLevelNameResolver::new();
        let mut parser = Parser::with_env("\"Hello, world!\"", &mut env);
        assert_eq!(AstExpression::parse(&mut parser), Ok(AstExpression::StrLit(AstStringLiteral { value: "Hello, world!".to_owned() })))
    }

    #[test]
    fn test_char() {
        let mut env = TopLevelNameResolver::new();
        let mut parser = Parser::with_env("'🦦'", &mut env);
        assert_eq!(AstExpression::parse(&mut parser), Ok(AstExpression::CharLit(AstCharLiteral { value: '🦦' })))
    }

    #[test]
    fn test_bool() {
        let mut env = TopLevelNameResolver::new();
        let mut parser = Parser::with_env("true false", &mut env);
        assert_eq!(AstExpression::parse(&mut parser), Ok(AstExpression::BoolLit(AstBoolLiteral { value: true })));
        assert_eq!(AstExpression::parse(&mut parser), Ok(AstExpression::BoolLit(AstBoolLiteral { value: false })));
    }

    #[test]
    fn test_number() {
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("31_415", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Int(
                31415, None)))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("31_415_usize", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Int(
                31415, Some(AstType::Base("usize".into())))))
        );

        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("3.14_15", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Float(
                3.1415, None)))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("3.14_15f32", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Float(
                3.1415, Some(AstType::Base("f32".into())))))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("31415e-4__f64", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Float(
                3.1415, Some(AstType::Base("f64".into())))))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("314.15e-2", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Float(
                3.1415, None)))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("0.031415e2", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Float(
                3.1415, None)))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("0.031415e+2", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Float(
                3.1415, None)))
        );
        assert_eq!(
            AstExpression::parse(&mut Parser::with_env("314.15e-2f32", &mut TopLevelNameResolver::new())),
            Ok(AstExpression::NumberLit(AstNumberLiteral::Float(
                3.1415, Some(AstType::Base("f32".into())))))
        );



        for ty in [
            "u8", "u16", "u32", "u64",
            "i8", "i16", "i32", "i64",
            "isize", "usize",
        ] {
            assert_eq!(
                AstExpression::parse(&mut Parser::with_env(&format!("42_{ty}"), &mut TopLevelNameResolver::new())),
                Ok(AstExpression::NumberLit(AstNumberLiteral::Int(
                    42, Some(AstType::Base(ty.into())))))
            );
        }
    }
}
