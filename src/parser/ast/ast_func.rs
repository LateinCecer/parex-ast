use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, SrcToken, Token};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_type::{AstType};
use crate::parser::ast::block::AstBlock;
use crate::parser::parameter_env::ParameterEnv;


#[derive(Debug, Clone)]
struct AstFuncParameter {
    name: String,
    ty: AstType,
    mutable: bool,
    pos: SrcPos,
}

#[derive(Debug, Clone)]
enum Receiver {
    Value { mutable: bool, pos: SrcPos, },
    Ptr { pos: SrcPos },
    MutPtr { pos: SrcPos },
}


#[derive(Debug, Clone)]
/// Contains the AST of a function signature
pub struct AstFuncSignature {
    pos: SrcPos,
    name: String,
    receiver: Option<Receiver>,
    params: Vec<AstFuncParameter>,
    env: ParameterEnv,
    ret: Option<AstType>,
    mutable: bool,
}


#[derive(Debug, Clone)]
pub struct AstFunc {
    sig: AstFuncSignature,
    body: AstBlock,
}


impl Receiver {
    fn pos(&self) -> &SrcPos {
        match self {
            Receiver::Value { pos, .. } => pos,
            Receiver::Ptr { pos } => pos,
            Receiver::MutPtr { pos } => pos,
        }
    }
}


impl Parsable for AstFunc {
    type Output = AstFunc;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let head = AstFuncSignature::parse(parser)?;
        let body = AstBlock::parse(parser)?;
        Ok(AstFunc {
            sig: head,
            body,
        })
    }
}

impl Display for AstFunc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.sig, self.body)
    }
}


impl AstFuncSignature {
    fn can_have_next(parser: &mut Parser) -> Result<(), ParseError> {
        match parser.peak() {
            Ok(local!(Token::Punct(Punct::Comma))) => {
                parser.next()?;
                Ok(())
            },
            Ok(local!(Token::Punct(Punct::BracketClose))) => Ok(()),
            Ok(_) => Err(ParseError::UnexpectedToken(
                Box::new(parser.next().unwrap()),
                "expected `,` or `)` after parameter declaration in function header".to_owned()
            )),
            Err(_) => Err(ParseError::LexError(parser.next().err().unwrap()))
        }
    }
}


impl Parsable for AstFuncSignature {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let mutable = if let Ok(local!(Token::Key(KeyWord::Mut))) = parser.peak() {
            parser.next()?;
            true
        } else {
            false
        };
        // find function keyword and function name
        let pos = expect_token!(parser; (Token::Key(KeyWord::Fn)), pos => pos
            expected "function keyword `fn`")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "function name identifier")?;
        let mut env = ParameterEnv::parse(parser)?;

        // parse parameters
        expect_token!(parser; (Token::Punct(Punct::BracketOpen))
            expected "function parameter list starting with `(`")?;

        // check for receiver parameter
        let mut receiver = match parser.peak() {
            Ok(local!(Token::Punct(Punct::Astrix))) => {
                parser.next()?;
                let m = expect_token!(parser;
                    (Token::Key(KeyWord::Mut)), pos => {
                        Receiver::MutPtr { pos }
                    };
                    (Token::Key(KeyWord::Const)), pos => {
                        Receiver::Ptr { pos }
                    }
                    expected "`const` or `mut` in receiver pointer definition")?;

                expect_token!(parser; (Token::Key(KeyWord::VarSelf))
                    expected "receiver keyword `self`")?;
                Self::can_have_next(parser)?;
                Some(m)
            },
            _ => None,
        };

        // read normal function parameters
        let mut params = vec![];
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BracketClose))) => {
                    parser.next()?;
                    break
                },
                Ok(_) => (),
                Err(_) => {
                    return Err(ParseError::LexError(parser.next().err().unwrap()));
                }
            }

            // read parameter
            let param = AstFuncParameter::parse(parser)?;
            match param {
                ParameterVariant::Parameter(param) => {
                    params.push(param);
                }
                ParameterVariant::Receiver(rec) => {
                    if params.is_empty() && receiver.is_none() {
                        receiver = Some(rec);
                    } else {
                        return Err(ParseError::UnexpectedToken(
                            Box::new(SrcToken::new(
                                Token::Key(KeyWord::VarSelf), *rec.pos())),
                            "`self` receiver parameter can only be used as the first \
                            parameter in a function".to_owned()));
                    }
                }
            }

            // check if there is a comma
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::Comma))) => {
                    parser.next()?;
                },
                Ok(local!(Token::Punct(Punct::BracketClose))) => {
                    parser.next()?;
                    break;
                },
                Ok(_) => return Err(ParseError::UnexpectedToken(
                    Box::new(parser.next().unwrap()),
                    "expected `,` or `)` after parameter declaration in function header".to_owned()
                )),
                Err(_) => {
                    return Err(ParseError::LexError(parser.next().err().unwrap()));
                }
            }
        }

        // parse return type
        let ret = if let Ok(local!(Token::Punct(Punct::RightArrow))) = parser.peak() {
            parser.next()?;
            Some(AstType::parse(parser)?)
        } else {
            None
        };

        // try to parse where clause if it exists
        if let Ok(local!(Token::Key(KeyWord::Where))) = parser.peak() {
            env.parse_where(parser)?;
        }

        Ok(AstFuncSignature {
            name,
            params,
            receiver,
            env,
            ret,
            mutable,
            pos,
        })
    }
}

impl Display for Receiver {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Receiver::Value { mutable: true, .. } => write!(f, "mut self"),
            Receiver::Value { mutable: false, .. } => write!(f, "self"),
            Receiver::Ptr { .. } => write!(f, "*const self"),
            Receiver::MutPtr { .. } => write!(f, "*mut self"),
        }
    }
}

impl Display for AstFuncSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}{}(", self.name, self.env)?;

        if let Some(rec) = &self.receiver {
            write!(f, "{}", rec)?;

            if !self.params.is_empty() {
                write!(f, ", ")?;
            }
        }

        let mut itr = self.params.iter();
        if let Some(par) = itr.next() {
            write!(f, "{par}")?;

            for par in itr {
                write!(f, ", {par}")?;
            }
        }
        write!(f, ") ")?;

        if let Some(ret) = &self.ret {
            write!(f, "-> {ret} ")?;
        }
        self.env.fmt_where(f)?;
        Ok(())
    }
}


enum ParameterVariant {
    Parameter(AstFuncParameter),
    Receiver(Receiver),
}

impl Parsable for AstFuncParameter {
    type Output = ParameterVariant;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let m = if let Ok(local!(Token::Key(KeyWord::Mut))) = parser.peak() {
            parser.next()?;
            true
        } else {
            false
        };

        expect_token!(parser;
            (Token::Ident(name)), pos => {
                // parse type
                expect_token!(parser; (Token::Punct(Punct::Colon))
                    expected "function parameter type starting with `:`")?;
                let ty = AstType::parse(parser)?;
                Ok(ParameterVariant::Parameter(AstFuncParameter { name, ty, mutable: m, pos }))
            };
            (Token::Key(KeyWord::VarSelf)), pos => {
                Ok(ParameterVariant::Receiver(Receiver::Value { mutable: m, pos }))
            }
            expected "parameter identifier")?
    }
}

impl Display for AstFuncParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.mutable {
            write!(f, "mut ")?;
        }

        write!(f, "{}: {}", self.name, self.ty)?;
        Ok(())
    }
}




#[cfg(test)]
mod test {
    use crate::parser::ast::ast_func::AstFunc;
    use crate::parser::ast::Parsable;
    use crate::parser::Parser;

    #[test]
    fn test_parse() {
        let src = r#"
        fn foo(mut bar: str, idx: usize) -> bool {
            idx == 0 && bar == "foo"
        }
        "#;

        let mut parser = Parser::new(src);
        let func = AstFunc::parse(&mut parser);

        assert!(func.is_ok());
        println!("{}", func.unwrap());
    }

    #[test]
    fn test_receiver() {
        let src = r#"
        fn foo(*const self, mut bar: str, idx: usize) -> bool {
            idx == 0 && bar == "foo"
        }
        "#;

        let mut parser = Parser::new(src);
        let func = AstFunc::parse(&mut parser);

        assert!(func.is_ok());
        println!("{}", func.unwrap());
    }

    #[test]
    fn test_mut_receiver() {
        let src = r#"
        fn foo(*mut self, mut bar: str, idx: usize) -> bool {
            idx == 0 && bar == "foo"
        }
        "#;

        let mut parser = Parser::new(src);
        let func = AstFunc::parse(&mut parser);

        assert!(func.is_ok());
        println!("{}", func.unwrap());
    }

    #[test]
    fn test_value_receiver() {
        let src = r#"
        fn foo(self, mut bar: str, idx: usize) -> bool {
            idx == 0 && bar == "foo"
        }
        "#;

        let mut parser = Parser::new(src);
        let func = AstFunc::parse(&mut parser);

        assert!(func.is_ok());
        println!("{}", func.unwrap());
    }

    #[test]
    fn test_mut_value_receiver() {
        let src = r#"
        fn foo(mut self, mut bar: str, idx: usize) -> bool {
            idx == 0 && bar == "foo"
        }
        "#;

        let mut parser = Parser::new(src);
        let func = AstFunc::parse(&mut parser);

        assert!(func.is_ok());
        println!("{}", func.unwrap());
    }
}
