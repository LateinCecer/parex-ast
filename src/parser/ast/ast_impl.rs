use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::ast_type::{AstType};
use crate::parser::ast::Parsable;
use crate::parser::parameter_env::ParameterEnv;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_func::AstFunc;
use crate::parser::ast::ast_type_def::AstTypeVariant;


#[derive(Debug, Clone)]
pub struct AstAssociatedTypeSig {
    pos: SrcPos,
    name: String,
}

#[derive(Debug, Clone)]
pub struct AstAssociatedType {
    head: AstAssociatedTypeSig,
    var: AstTypeVariant,
}

#[derive(Debug, Clone)]
pub struct AstImpl {
    pos: SrcPos,
    env: ParameterEnv,
    ty: AstType,
    f: Option<AstType>,
    body: AstImplBody,
}

#[derive(Debug, Clone)]
struct AstImplBody {
    funcs: Vec<AstFunc>,
    types: Vec<AstAssociatedType>,
}



impl Parsable for AstAssociatedType {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let sig = AstAssociatedTypeSig::parse(parser)?;
        expect_token!(parser; (Token::Punct(Punct::Assign))
            expected "associated type assignment starting with `=`")?;

        let var = AstTypeVariant::parse(parser)?;
        Ok(AstAssociatedType {
            head: sig, var
        })
    }
}

impl Parsable for AstAssociatedTypeSig {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Type)), pos => pos
            expected "associated type definition starting with `type`")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "associated type name")?;
        Ok(AstAssociatedTypeSig {
            name, pos
        })
    }
}

impl Parsable for AstImplBody {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "implementation body starting with `{`")?;

        let mut funcs = vec![];
        let mut types = vec![];
        loop {
            if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                break;
            }
            // try to parse either a function, or a type
            match parser.peak() {
                Ok(local!(Token::Key(KeyWord::Fn))) => {
                    funcs.push(AstFunc::parse(parser)?);
                }
                Ok(local!(Token::Key(KeyWord::Type))) => {
                    types.push(AstAssociatedType::parse(parser)?);
                },
                Ok(local!(Token::Punct(Punct::Semicolon))) => {
                    parser.next()?;
                },
                Ok(_) => return Err(ParseError::UnexpectedToken(
                    Box::new(parser.next().unwrap()),
                    "`impl`s can only contain `type` or `fn` definitions".to_owned()
                )),
                Err(_) => return Err(ParseError::LexError(parser.next().err().unwrap()))
            }
        }

        Ok(AstImplBody {
            funcs,
            types,
        })
    }
}

impl Parsable for AstImpl {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Impl)), pos => pos
            expected "`impl` keyword")?;

        let mut env = ParameterEnv::parse(parser)?;
        let p1 = AstType::parse(parser)?;

        // check for `for` implementation
        let (ty, f) = if let Ok(local!(Token::Key(KeyWord::For))) = parser.peak() {
            parser.next()?;
            let p2 = AstType::parse(parser)?;
            (p2, Some(p1))
        } else {
            (p1, None)
        };

        // impl where-clause
        if let Ok(local!(Token::Key(KeyWord::Where))) = parser.peak() {
            env.parse_where(parser)?;
        }
        // parse body
        let body = AstImplBody::parse(parser)?;

        Ok(AstImpl {
            pos,
            env,
            ty,
            f,
            body,
        })
    }
}

impl Display for AstAssociatedTypeSig {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "type {}", self.name)
    }
}

impl Display for AstAssociatedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = {}", self.head, self.var)
    }
}

impl Display for AstImplBody {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;

        for ty in self.types.iter() {
            writeln!(f, "    {}", ty)?;
        }
        for func in self.funcs.iter() {
            writeln!(f, "    {}", func)?;
        }

        write!(f, "}}")
    }
}

impl Display for AstImpl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(ft) = &self.f {
            write!(f, "impl{} {} for {}", self.env, ft, self.ty)?;
        } else {
            write!(f, "impl{} {}", self.env, self.ty)?;
        }
        self.env.fmt_where(f)?;
        write!(f, "{}", self.body)
    }
}
