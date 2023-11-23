use std::fmt::{Display, Formatter};
use std::ops::{Deref, DerefMut};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::lexer::*;
use crate::parser::ast::expression::{AstExpression, AstTyped};
use crate::parser::r#type::{Lifetime, Type};
use crate::parser::r#type::core::{CORE_F32, CORE_F64, CORE_I16, CORE_I32, CORE_I64, CORE_I8, CORE_ISIZE, CORE_U16, CORE_U32, CORE_U64, CORE_U8, CORE_USIZE};



#[derive(Clone, Debug, PartialEq)]
pub struct TypeName {
    path: Vec<TypeNameEntry>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeNameEntry {
    pub name: String,
    pub params: ParameterList,
    colon: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParameterList {
    pos: Option<SrcPos>,
    params: Vec<AstType>,
}

impl TypeName {
    pub fn parse_with_string(name: String, parser: &mut Parser, require_colon: bool) -> Result<Self, ParseError> {
        Self::parse_from_first(
            TypeNameEntry::parse_from_name(name, parser, require_colon)?,
            parser,
            require_colon,
        )
    }

    /// Parses a type name from an initial type name segment.
    /// If a double colon `::` is not required between the segment name and its potential parameter
    /// list, a simple type name can be parsed without the `::` punct.
    /// However, these simple type names can contain **only one** segment.
    fn parse_from_first(first: TypeNameEntry, parser: &mut Parser, mut require_colon: bool) -> Result<Self, ParseError> {
        if require_colon && !first.colon {
            panic!();
        }
        require_colon = first.colon;
        let mut names = vec![first];

        if require_colon {
            while let Ok(local!(Token::Punct(Punct::DoubleColon))) = parser.peak() {
                parser.next()?;
                names.push(TypeNameEntry::parse(parser, true)?);
            }
        }
        Ok(TypeName {
            path: names,
        })
    }
}

impl Parsable for TypeName {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        Self::parse_from_first(
            TypeNameEntry::parse(parser, true)?,
            parser,
            true
        )
    }
}

impl ParameterList {
    pub fn empty() -> Self {
        ParameterList {
            pos: None,
            params: Vec::new(),
        }
    }
}

impl TypeNameEntry {
    pub fn from_ident(ident: String, parser: &mut Parser) -> Result<Self, ParseError> {
        Ok(TypeNameEntry {
            name: ident,
            params: ParameterList::parse(parser)?,
            colon: false,
        })
    }
}

impl TypeNameEntry {
    fn parse_from_name(name: String, parser: &mut Parser, require_colon: bool) -> Result<Self, ParseError> {
        match parser.peak() {
            Ok(local!(Token::Punct(Punct::DoubleColon))) => {
                let colon_pos = parser.next()?.pos;
                let params = if let Ok(local!(Token::Punct(Punct::Lt))) = parser.peak() {
                    // parse parameters and continue
                    ParameterList::parse(parser)?
                } else {
                    // reset parser to colon position, since that should not have been popped
                    parser.reset_until(&colon_pos)?;
                    ParameterList::empty()
                };

                Ok(TypeNameEntry {
                    name,
                    params,
                    colon: true,
                })
            },
            Ok(_) if require_colon => {
                Ok(TypeNameEntry {
                    name,
                    params: ParameterList::empty(),
                    colon: true,
                })
            },
            Ok(_) => {
                Ok(TypeNameEntry {
                    name,
                    params: ParameterList::parse(parser)?,
                    colon: false,
                })
            },
            Err(_) => Err(ParseError::LexError(parser.next().err().unwrap())),
        }
    }


    fn parse(parser: &mut Parser, require_colon: bool) -> Result<Self, ParseError> {
        let name = expect_token!(parser;
            (Token::Ident(name)) => name;
            (Token::Key(KeyWord::TySelf)) => { "Self".to_owned() }
            expected "type name identifier")?;

        // check for double colon
        Self::parse_from_name(name, parser, require_colon)
    }
}

impl Display for TypeNameEntry {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}::{}", self.name, self.params)
    }
}


impl Parsable for ParameterList {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        if let Ok(local!(Token::Punct(Punct::Lt))) = parser.peak() {
            let pos = Some(parser.next()?.pos);

            let mut params = Vec::new();
            loop {
                if let Ok(local!(Token::Punct(Punct::Gt))) = parser.peak() {
                    parser.next()?;
                    break Ok(ParameterList { pos, params });
                }
                // read item
                params.push(AstType::parse(parser)?);
                // check if next item can be parsed
                match parser.peak() {
                    Ok(local!(Token::Punct(Punct::Comma))) => {
                        parser.next()?;
                    },
                    Ok(local!(Token::Punct(Punct::Gt))) => (),
                    Ok(_) => return Err(ParseError::UnexpectedToken(
                        Box::new(parser.next().unwrap()),
                        "expected trailing parameters starting with `,` or end of parameter \
                        list with `>`".to_owned()
                    )),
                    Err(_) => return Err(ParseError::LexError(parser.next().err().unwrap())),
                }
            }
        } else {
            Ok(ParameterList { pos: None, params: vec![] })
        }
    }
}

impl Display for ParameterList {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.params.is_empty() {
            return Ok(())
        }

        write!(f, "<")?;
        let mut iter = self.params.iter();
        write!(f, "{}", iter.next().unwrap())?;
        for param in iter {
            write!(f, ", {}", param)?;
        }
        write!(f, ">")
    }
}


impl Display for TypeName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.path.iter();
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

impl From<String> for TypeName {
    fn from(value: String) -> Self {
        let s = value.split("::");
        let path: Vec<_> = s.map(|s| s.to_owned())
            .collect();
        path.into()
    }
}

impl From<&str> for TypeName {
    fn from(value: &str) -> Self {
        let s = value.split("::");
        let path: Vec<_> = s.map(|s| s.to_owned())
            .collect();
        path.into()
    }
}

impl From<Vec<String>> for TypeName {
    fn from(value: Vec<String>) -> Self {
        TypeName {
            path: value.iter()
                .map(|s| TypeNameEntry::from(s.clone()))
                .collect()
        }
    }
}

impl From<String> for TypeNameEntry {
    fn from(value: String) -> Self {
        TypeNameEntry {
            name: value,
            params: ParameterList::empty(),
            colon: false,
        }
    }
}

impl Deref for TypeName {
    type Target = Vec<TypeNameEntry>;

    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl DerefMut for TypeName {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.path
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum AstType {
    Base(TypeName),
    Ptr(Box<AstType>),
    MutPtr(Box<AstType>),
    Array(Box<AstType>, Box<AstExpression>),
    Slice(Box<AstType>),
}

impl AstType {
    pub fn is_integer(&self) -> bool {
        matches!(self, Self::Base(s) if *s == CORE_U8.into() || *s == CORE_U16.into() || *s == CORE_U32.into()
                || *s == CORE_U64.into() || *s == CORE_USIZE.into() || *s == CORE_I8.into() || *s == CORE_I16.into()
                || *s == CORE_I32.into() || *s == CORE_I64.into() || *s == CORE_ISIZE.into())
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::Base(s) if *s == CORE_F32.into() || *s == CORE_F64.into())
    }
}


impl Parsable for AstType {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let err = "type specifier (`*const`, `*mut` or `ident`)".to_owned();

        match parser.next() {
            Ok(SrcToken { token: Token::Punct(Punct::Astrix), pos }) => {
                // parse pointer
                match parser.next() {
                    Ok(local!(Token::Key(KeyWord::Mut))) => Ok(AstType::MutPtr(Box::new(Self::parse(parser)?))),
                    Ok(local!(Token::Key(KeyWord::Const))) => Ok(AstType::Ptr(Box::new(Self::parse(parser)?))),
                    Ok(t) => Err(ParseError::UnexpectedToken(
                        Box::new(t),
                        "'const' or 'mut' in pointer type".to_owned())),
                    Err(e) => Err(e.into()),
                }
            }
            Ok(local!(Token::Punct(Punct::SquareOpen))) => {
                let base = AstType::parse(parser)?;
                match parser.next() {
                    Ok(local!(Token::Punct(Punct::Semicolon))) => {
                        // parse as array
                        let len = AstExpression::parse(parser)?;
                        expect_token!(parser; (Token::Punct(Punct::SquareClose))
                            expected "to find closing statement in array type")?;
                        Ok(AstType::Array(Box::new(base), Box::new(len)))
                    }
                    Ok(local!(Token::Punct(Punct::SquareClose))) => {
                        // parse as slice
                        Ok(AstType::Slice(Box::new(base)))
                    }
                    Ok(t) => Err(ParseError::UnexpectedToken(
                        Box::new(t),
                        "`;` or `]` in array or slice type".to_owned())),
                    Err(e) => Err(ParseError::LexError(e))
                }
            }
            Ok(local!(Token::Ident(name))) => {
                // parse base type
                Ok(AstType::Base(TypeName::parse_with_string(name, parser, false)?))
            }
            Ok(local!(Token::Key(KeyWord::TySelf))) => {
                // parse base type
                Ok(AstType::Base(TypeName::parse_with_string("Self".to_string(), parser, false)?))
            }
            Ok(t) => Err(ParseError::UnexpectedToken(
                Box::new(t), err)),
            Err(LexError::EndOfStream(pos)) => Err(
                ParseError::UnexpectedEndOfStream(pos, err)),
            Err(e) => Err(ParseError::LexError(e)),
        }
    }
}

impl AstType {
    pub fn make(self, parser: &mut Parser) -> Result<Type, ParseError> {
        let scope = *parser.env.current_scope()?;
        match self {
            AstType::Base(name) => {
                let s = parser.env.find_struct(&name)
                    .ok_or(ParseError::UnknownTypeName(
                        name.clone(),
                        format!("Could not find type with name '{name}' \
                        in current scope. Make sure all dependencies are included correctly")))?;
                Ok(Type::base(s, Lifetime::Scope(scope)))
            }
            AstType::Ptr(base) => {
                Ok(Type::ptr(base.make(parser)?, Lifetime::Scope(scope)))
            }
            AstType::MutPtr(base) => {
                Ok(Type::mut_ptr(base.make(parser)?, Lifetime::Scope(scope)))
            }
            AstType::Array(base, len) => {
                Ok(Type::array(
                    base.make(parser)?,
                    len.static_eval(parser).and_then(|val| val.as_u64())
                        .ok_or(ParseError::ComptimeEval(len))? as usize,
                    Lifetime::Scope(scope)
                ))
            }
            AstType::Slice(base) => {
                Ok(Type::slice(base.make(parser)?, Lifetime::Scope(scope)))
            }
        }
    }
}

impl Display for AstType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstType::Base(base) => write!(f, "{base}"),
            AstType::Ptr(base) => write!(f, "*const {base}"),
            AstType::MutPtr(base) => write!(f, "*mut {base}"),
            AstType::Array(base, len) => write!(f, "[{base}; {len}]"),
            AstType::Slice(base) => write!(f, "[{base}]"),
        }
    }
}


#[cfg(test)]
mod test {
    use crate::parser::ast::ast_type::AstType;
    use crate::parser::ast::Parsable;
    use crate::parser::Parser;

    #[test]
    fn test_ptr() {
        assert_eq!(
            AstType::parse(&mut Parser::new("*const usize")),
            Ok(AstType::Ptr(Box::new(AstType::Base("usize".into()))))
        );
        assert_eq!(
            AstType::parse(&mut Parser::new("*mut usize")),
            Ok(AstType::MutPtr(Box::new(AstType::Base("usize".into()))))
        );
        assert_eq!(
            AstType::parse(&mut Parser::new("usize")),
            Ok(AstType::Base("usize".into()))
        );
        assert_eq!(
            AstType::parse(&mut Parser::new("[usize]")),
            Ok(AstType::Slice(Box::new(AstType::Base("usize".into()))))
        );
        assert_eq!(
            AstType::parse(&mut Parser::new("*const [usize]")),
            Ok(AstType::Ptr(Box::new(AstType::Slice(Box::new(AstType::Base("usize".into()))))))
        );
        // assert_eq!(
        //     AstType::parse(&mut Parser::new("*const usize")),
        //     Ok(AstType::Ptr(Box::new(AstType::Base("usize".to_owned()))))
        // );
    }
}
