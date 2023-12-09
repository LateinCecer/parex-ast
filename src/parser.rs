mod resolver;
mod r#type;
mod func;
mod structs;
mod ast;
mod parameter_env;
mod pre_parse;
mod file_parser;
mod module_parser;

use std::fmt::{Display, Formatter};
use std::{io, mem};
use std::rc::Rc;
use crate::lexer::{Lexer, LexError, Punct, SrcPos, SrcToken, Token};


#[derive(Clone, Debug)]
pub enum ParseError {
    UnexpectedToken(Box<SrcToken>, String),
    UnexpectedEndOfStream(SrcPos, String),
    LexError(LexError),
    TypeError(TypeError),
    ResolveError(ResolveError),
    UnknownTypeName(TypeName, String),
    ComptimeEval(Box<AstExpression>),
    NumberConversionError(Box<SrcToken>, String),
    IoError(Rc<io::Error>),
    FileError(FileError)
}

impl PartialEq for ParseError {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ParseError::UnexpectedToken(src, s) => {
                match other {
                    ParseError::UnexpectedToken(src_other, s_other) => {
                        src == src_other && s == s_other
                    },
                    _ => false,
                }
            }
            ParseError::UnexpectedEndOfStream(src, s) => {
                match other {
                    ParseError::UnexpectedEndOfStream(src_other, s_other) => {
                        src == src_other && s == s_other
                    },
                    _ => false,
                }
            }
            ParseError::LexError(e) => {
                match other {
                    ParseError::LexError(e_other) => e == e_other,
                    _ => false,
                }
            }
            ParseError::TypeError(e) => {
                match other {
                    ParseError::TypeError(e_other) => e == e_other,
                    _ => false,
                }
            }
            ParseError::ResolveError(_e) => {
                false
            }
            ParseError::UnknownTypeName(name, s) => {
                match other {
                    ParseError::UnknownTypeName(name_other, s_other) => {
                        name == name_other && s == s_other
                    },
                    _ => false,
                }
            }
            ParseError::ComptimeEval(expr) => {
                match other {
                    ParseError::ComptimeEval(expr_other) => expr == expr_other,
                    _ => false,
                }
            }
            ParseError::NumberConversionError(src, s) => {
                match other {
                    ParseError::NumberConversionError(src_other, s_other) => {
                        src == src_other && s == s_other
                    },
                    _ => false,
                }
            }
            ParseError::IoError(e) => {
                false
            }
            ParseError::FileError(_) => {
                false
            }
        }
    }
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        ParseError::LexError(value)
    }
}

impl From<TypeError> for ParseError {
    fn from(value: TypeError) -> Self {
        ParseError::TypeError(value)
    }
}

impl From<ResolveError> for ParseError {
    fn from(value: ResolveError) -> Self {
        ParseError::ResolveError(value)
    }
}


pub type ParseResult<T> = Result<T, ParseError>;


impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken(token, msg) => {
                write!(f, "Unexpected token '{}' at {}: expected {msg}", token.token, token.pos)
            }
            ParseError::UnexpectedEndOfStream(pos, msg) => {
                write!(f, "Unexpected end of stream at {pos}: expected {msg}")
            }
            ParseError::LexError(err) => {
                write!(f, "Error '{err}' in token generation")
            }
            ParseError::TypeError(err) => {
                write!(f, "Error '{err}' in type system")
            }
            ParseError::ResolveError(err) => {
                write!(f, "Error '{err}' in name resolution")
            }
            ParseError::UnknownTypeName(name, msg) => {
                write!(f, "Unable to resolve type with name '{name}': expected {msg}")
            }
            ParseError::ComptimeEval(expr) => {
                write!(f, "Expression {expr:?} cannot be evaluated at compiletime")
            }
            ParseError::NumberConversionError(token, expected) => {
                write!(f, "Failed to convert number literal token '{}' at {} to {expected}", token.token, token.pos)
            }
            ParseError::IoError(e) => {
                write!(f, "I/O error: {e}")
            }
            ParseError::FileError(e) => {
                write!(f, "File error: {e}")
            }
        }
    }
}


pub struct Parser<'a, 'env> {
    src: &'a str,
    lexer: Lexer<'a>,
    current: Result<SrcToken, LexError>,
    pub env: &'env mut TopLevelNameResolver,
}


macro_rules! local(
    ($($token:tt)+) => (crate::lexer::SrcToken { token: $($token)+, pos: _ });
    (pos, $($token:tt)+) => (crate::lexer::SrcToken { token: $($token)+, pos });
);

pub (crate) use local;


macro_rules! expect_token(
    ($p:expr; $($cond:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok(()),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; @ $($cond:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok(t @ crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok(t),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt if $cond2:tt)+ expected $err:expr) => (
        match $p.next() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: _ }) if $cond2 => Ok(()),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; @ $($cond:tt if $cond2:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok(t @ crate::lexer::SrcToken { token: $cond, pos: _ }) if $cond2 => Ok(t),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt, $pos:tt if $cond2:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: $pos }) if $cond2 => Ok(()),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $($t:ident @ $cond:tt if $cond2:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: _ }) if $cond2 => Ok(t),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt => $val:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $t:ident @ $($cond:tt => $val:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: _ }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt, $pos:tt => $val:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: $pos }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $t:ident @ $($cond:tt, $pos:tt => $val:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: $pos }) => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );

    ($p:expr; $($cond:tt, $pos:tt if $cond2:tt => $val:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok(crate::lexer::SrcToken { token: $cond, pos: $pos }) if $cond2 => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
    ($p:expr; $t:ident @ $($cond:tt, $pos:tt if $cond2:tt => $val:tt);+ expected $err:expr) => (
        match $p.next() {
            $(Ok($t @ crate::lexer::SrcToken { token: $cond, pos: $pos }) if $cond2 => Ok($val),)+
            Ok(t) => Err(crate::parser::ParseError::UnexpectedToken(Box::new(t), $err.to_owned())),
            Err(crate::lexer::LexError::EndOfStream(pos)) => Err(crate::parser::ParseError::UnexpectedEndOfStream(pos, $err.to_owned())),
            Err(e) => Err(e.into()),
        }
    );
);

pub (crate) use expect_token;
use crate::parser::ast::ast_type::TypeName;
use crate::parser::ast::expression::AstExpression;
use crate::parser::r#type::TypeError;
use crate::parser::resolver::{TopLevelNameResolver, ResolveError};



macro_rules! skip_block(
    ($parser:expr, $open:ident, $close:ident) => ({
        expect_token!($parser; (Token::Punct(Punct::$open))
            expected "block")?;

        let mut count = 1_usize;
        while count > 0 {
            match $parser.next() {
                Ok(local!(Token::Punct(Punct::$open))) => count += 1,
                Ok(local!(Token::Punct(Punct::$close))) => count -= 1,
                Err(e) => return Err(ParseError::LexError(e)),
                _ => (),
            }
        }
        Ok(())
    });
);

macro_rules! skip_to(
    ($parser:expr, $to:tt) => ({
        loop {
            match $parser.peak() {
                Ok(crate::lexer::SrcToken { token: $to, .. }) => break,
                _ => { $parser.next().map_err(|e: crate::lexer::LexError| ParseError::LexError(e))?; },
            }
        }
        Ok::<(), ParseError>(())
    });
);
pub (crate) use skip_to;
use crate::parser::module_parser::FileError;


impl<'a, 'env> Parser<'a, 'env> {
    pub fn with_env(src: &'a str, env: &'env mut TopLevelNameResolver) -> Self {
        let mut parser = Parser {
            src,
            lexer: Lexer::new(src),
            current: Err(LexError::EndOfStream(SrcPos::default())),
            env,
        };
        let _ = parser.next();
        parser
    }

    pub fn pos(&self) -> &SrcPos {
        match &self.current {
            Ok(SrcToken { pos, .. }) => pos,
            _ => &self.lexer.pos,
        }
    }

    /// Skips all tokens in the current token stream, until the position `pos` in the source code
    /// is reached.
    /// Since this action reads tokens from the token stream, it might lead to a `LexError` if the
    /// source position cannot be reached.
    /// This method should therefore only be used when it is certain, that `pos` is a valid position
    /// within the current token stream.
    pub fn skip_until(&mut self, pos: &SrcPos) -> Result<(), LexError> {
        while self.pos() != pos {
            self.next()?;
        }
        Ok(())
    }

    pub fn peak(&self) -> &Result<SrcToken, LexError> {
        &self.current
    }

    /// Returns the next token in the token stream.
    pub fn next(&mut self) -> Result<SrcToken, LexError> {
        let mut n = self.lexer.next_token();
        mem::swap(&mut self.current, &mut n);
        n
    }

    pub fn skip_brace(&mut self) -> Result<(), ParseError> {
        skip_block!(self, BraceOpen, BraceClose)
    }

    pub fn skip_bracket(&mut self) -> Result<(), ParseError> {
        skip_block!(self, BracketOpen, BracketClose)
    }

    pub fn skip_env(&mut self) -> Result<(), ParseError> {
        if let Ok(local!(Token::Punct(Punct::Lt))) = self.peak() {
            skip_block!(self, Lt, Gt)
        } else {
            Ok(())
        }
    }

    /// Resets the token stream
    pub fn reset_ts(&mut self) {
        self.lexer = Lexer::new(self.src);
        self.current = Err(LexError::EndOfStream(SrcPos::default()));
        let _ = self.next(); // prime parser
    }

    /// Resets the token stream and skips to the specified position.
    /// This should be the preferred method to position the token stream to a specific location in
    /// the code.
    pub fn reset_until(&mut self, pos: &SrcPos) -> Result<(), LexError> {
        self.reset_ts();
        self.skip_until(pos)
    }
}
