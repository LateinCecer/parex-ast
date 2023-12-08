use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::ast_func::AstFuncSignature;
use crate::parser::ast::ast_impl::AstAssociatedTypeSig;
use crate::parser::ast::Parsable;
use crate::parser::parameter_env::ParameterEnv;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::resolver::{ItemVariant, ResolveError};

#[derive(Debug, Clone)]
pub struct AstTrait {
    name: String,
    pos: SrcPos,
    env: ParameterEnv,
    associated_types: Vec<AstAssociatedTypeSig>,
    funcs: Vec<AstFuncSignature>,
}


impl Parsable for AstTrait {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Trait)), pos => pos
            expected "trait definition starting with `trait` keyword")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "trait name identifier")?;

        // read parameter environment
        let mut env = ParameterEnv::parse(parser)?;
        if let Ok(local!(Token::Key(KeyWord::Where))) = parser.peak() {
            env.parse_where(parser)?;
        }

        expect_token!(parser; (Token::Punct(Punct::BraceOpen))
            expected "trait definition body, starting with `{`")?;

        // remember current scope
        let current_scope = *parser.env.current_scope()?;
        // find scope for trait
        let item = parser.env.find_item(&name.clone().into())
            .ok_or(ParseError::ResolveError(ResolveError::UnknownItem(name.clone().into())))?;
        if item.variant != ItemVariant::Trait {
            return Err(ParseError::ResolveError(ResolveError::InvalidUse(name.clone().into())));
        }
        let scope = item.scope;
        parser.env.revert_to_scope(&scope);


        let mut associated_types = vec![];
        let mut funcs = vec![];
        loop {
            if let Ok(local!(Token::Punct(Punct::BraceClose))) = parser.peak() {
                break;
            }
            // try to parse either a function, or a type
            match parser.peak() {
                Ok(local!(Token::Key(KeyWord::Fn))) => {
                    // push function signature to environment
                    let sig = AstFuncSignature::parse(parser)?;
                    sig.push_to_env(parser);
                    funcs.push(sig);
                }
                Ok(local!(Token::Key(KeyWord::Type))) => {
                    // add associated type signature to the environment
                    let sig = AstAssociatedTypeSig::parse(parser)?;
                    sig.push_to_env(parser)?;
                    associated_types.push(sig);
                },
                Ok(local!(Token::Punct(Punct::Semicolon))) => {
                    parser.next()?;
                },
                Ok(_) => return Err(ParseError::UnexpectedToken(
                    Box::new(parser.next().unwrap()),
                    "`traits`s can only contain `type` or `fn` signatures".to_owned()
                )),
                Err(_) => return Err(ParseError::LexError(parser.next().err().unwrap()))
            }
        }

        // reset scope of the parser env
        parser.env.revert_to_scope(&current_scope);

        // return stuff
        Ok(AstTrait {
            name,
            pos,
            env,
            associated_types,
            funcs
        })
    }
}

impl Display for AstTrait {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "trait {}{}", self.name, self.env)?;
        self.env.fmt_where(f)?;
        writeln!(f, "{{")?;

        for ty in self.associated_types.iter() {
            writeln!(f, "{}", ty)?;
        }
        for func in self.funcs.iter() {
            writeln!(f, "{}", func)?;
        }

        writeln!(f, "}}")
    }
}
