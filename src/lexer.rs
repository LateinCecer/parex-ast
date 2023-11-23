use std::fmt::{Display, Formatter};
use std::mem;
use std::ops::Deref;
use std::str::Chars;

#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Default)]
pub struct SrcPos {
    pub line: usize,
    pub col: usize,
}

impl SrcPos {
    pub fn new(line: usize, col: usize) -> SrcPos {
        SrcPos {
            line,
            col,
        }
    }
}

impl Display for SrcPos {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {} col {}", self.line, self.col)
    }
}


#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
pub enum LexError {
    UnexpectedSymbol(char, SrcPos),
    EndOfStream(SrcPos),
    InvalidNumber(String, SrcPos),
    InvalidChar(String, SrcPos),
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnexpectedSymbol(sym, pos) => {
                write!(f, "Unexpected symbol '{sym}' @ {pos}")
            }
            LexError::EndOfStream(pos) => {
                write!(f, "End of stream @ {pos}")
            }
            LexError::InvalidNumber(s, pos) => {
                write!(f, "Invalid number literal '{s}' @ {pos}")
            }
            LexError::InvalidChar(s, pos) => {
                write!(f, "Invalid char formatting '{s}' @ {pos}")
            }
        }
    }
}


#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug)]
pub enum KeyWord {
    Fn,
    For,
    Struct,
    Enum,
    Const,
    Mut,
    Let,
    Switch,
    If,
    Else,
    Loop,
    Return,
    Break,
    Continue,
    Impl,
    Trait,
    TySelf,
    VarSelf,
    As,
    Where,
    Type,
}

impl Display for KeyWord {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            KeyWord::Fn => write!(f, "fn"),
            KeyWord::For => write!(f, "for"),
            KeyWord::Struct => write!(f, "struct"),
            KeyWord::Enum => write!(f, "enum"),
            KeyWord::Const => write!(f, "const"),
            KeyWord::Mut => write!(f, "mut"),
            KeyWord::Let => write!(f, "let"),
            KeyWord::Switch => write!(f, "switch"),
            KeyWord::If => write!(f, "if"),
            KeyWord::Else => write!(f, "else"),
            KeyWord::Loop => write!(f, "loop"),
            KeyWord::Return => write!(f, "return"),
            KeyWord::Break => write!(f, "break"),
            KeyWord::Continue => write!(f, "continue"),
            KeyWord::Impl => write!(f, "impl"),
            KeyWord::Trait => write!(f, "trait"),
            KeyWord::TySelf => write!(f, "Self"),
            KeyWord::VarSelf => write!(f, "self"),
            KeyWord::As => write!(f, "as"),
            KeyWord::Where => write!(f, "where"),
            KeyWord::Type => write!(f, "type"),
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug)]
pub enum Punct {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Astrix,
    /// `/`
    Slash,
    /// `%`
    Rem,

    /// `&`
    And,
    /// `|`
    Or,
    /// `^`
    Xor,

    /// `&&`
    LAnd,
    /// `||`
    LOr,
    /// `^^`
    LXor,

    /// `!`
    Not,

    /// `=`
    Assign,

    /// `==`
    Eq,
    /// `!=`
    Neq,
    /// `<`
    Lt,
    /// `>`
    Gt,
    /// `<=`
    Leq,
    /// `>=`
    Geq,

    /// `,`
    Comma,
    /// `:`
    Colon,
    /// `;`
    Semicolon,
    /// `::`
    DoubleColon,
    /// `.`
    Dot,
    /// `(`
    BracketOpen,
    /// `)`
    BracketClose,
    /// `{`
    BraceOpen,
    /// `}`
    BraceClose,
    /// `[`
    SquareOpen,
    /// `]`
    SquareClose,

    /// `->`
    RightArrow,
    /// `=>`
    BigRightArrow,

    /// `<<`
    Lst,
    /// `>>`
    Rst,
}

impl Display for Punct {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Punct::Plus => write!(f, "+"),
            Punct::Minus => write!(f, "-"),
            Punct::Astrix => write!(f, "*"),
            Punct::Slash => write!(f, "/"),
            Punct::Rem => write!(f, "%"),
            Punct::And => write!(f, "&"),
            Punct::Or => write!(f, "|"),
            Punct::Xor => write!(f, "^"),
            Punct::LAnd => write!(f, "&&"),
            Punct::LOr => write!(f, "||"),
            Punct::LXor => write!(f, "^^"),
            Punct::Not => write!(f, "!"),
            Punct::Assign => write!(f, "="),
            Punct::Eq => write!(f, "=="),
            Punct::Neq => write!(f, "!="),
            Punct::Lt => write!(f, "<"),
            Punct::Gt => write!(f, ">"),
            Punct::Leq => write!(f, "<="),
            Punct::Geq => write!(f, ">="),
            Punct::Comma => write!(f, ","),
            Punct::Colon => write!(f, ":"),
            Punct::Semicolon => write!(f, ";"),
            Punct::DoubleColon => write!(f, "::"),
            Punct::Dot => write!(f, "."),
            Punct::BracketOpen => write!(f, "("),
            Punct::BracketClose => write!(f, ")"),
            Punct::BraceOpen => write!(f, "{{"),
            Punct::BraceClose => write!(f, "}}"),
            Punct::SquareOpen => write!(f, "["),
            Punct::SquareClose => write!(f, "]"),
            Punct::RightArrow => write!(f, "->"),
            Punct::BigRightArrow => write!(f, "=>"),
            Punct::Lst => write!(f, "<<"),
            Punct::Rst => write!(f, ">>"),
        }
    }
}

impl Punct {
    pub fn precedence(&self) -> Option<usize> {
        match self {
            Punct::Plus | Punct::Minus => Some(11000),
            Punct::Astrix | Punct::Slash | Punct::Rem => Some(12000),

            Punct::And => Some(9000),
            Punct::Or => Some(7000),
            Punct::Xor => Some(8000),
            Punct::LAnd => Some(5000),
            Punct::LOr => Some(4000),
            Punct::LXor => Some(4000),
            Punct::Not => None,
            Punct::Assign => Some(2000),
            Punct::Eq | Punct::Neq | Punct::Lt | Punct::Gt | Punct::Leq | Punct::Geq => Some(6000),

            Punct::Comma => None,
            Punct::Colon => None,
            Punct::Semicolon => None,
            Punct::DoubleColon => None,
            Punct::Dot => Some(17000),
            Punct::BracketOpen => Some(16000),
            Punct::BracketClose => None,
            Punct::BraceOpen => None,
            Punct::BraceClose => None,
            Punct::SquareOpen => Some(16000),
            Punct::SquareClose => None,
            Punct::RightArrow => None,
            Punct::BigRightArrow => None,
            Punct::Lst | Punct::Rst => Some(10000),
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
pub struct NumWithLeading(pub usize, pub usize);

impl Display for NumWithLeading {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", "0".repeat(self.1), self.0)
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Debug)]
pub enum Token {
    Punct(Punct),
    Ident(String),
    Key(KeyWord),
    NumLiteral(usize, Option<NumWithLeading>, Option<isize>, Option<String>),
    StrLiteral(String),
    CharLiteral(char),
    Comment(String),
}

impl Token {
    pub fn localize(self, pos: SrcPos) -> SrcToken {
        SrcToken::new(self, pos)
    }
}

#[derive(Ord, PartialOrd, PartialEq, Eq, Clone, Debug)]
pub struct SrcToken {
    pub token: Token,
    pub pos: SrcPos,
}

impl SrcToken {
    pub fn new(token: Token, pos: SrcPos) -> Self {
        SrcToken {
            token, pos
        }
    }
}

impl Deref for SrcToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}


impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Punct(punct) => {
                write!(f, "Punctuation '{punct}'")
            }
            Token::Ident(ident) => {
                write!(f, "Identifier '{ident}'")
            }
            Token::Key(key) => {
                write!(f, "Keyword '{key}'")
            }
            Token::NumLiteral(x, y, z, hint) => {
                write!(f, "Number Literal {x}")?;
                if let Some(y) = y {
                    write!(f, ".{y}")?;
                }
                if let Some(z) = z {
                    write!(f, "e{z}")?;
                }
                if let Some(hint) = hint {
                    write!(f, " {hint}")?;
                }
                Ok(())
            }
            Token::StrLiteral(lit) => {
                write!(f, "String Literal \"{lit}\"")
            }
            Token::CharLiteral(lit) => {
                write!(f, "Char Literal '{lit}'")
            }
            Token::Comment(comment) => {
                write!(f, "Comment '{comment}'")
            }
        }
    }
}

pub struct Lexer<'a> {
    chars: Chars<'a>,
    pub pos: SrcPos,
    next_pos: SrcPos,
    current: Option<char>,
}

macro_rules! expect_char (
    ($self:expr; is $cond:literal => $t1:expr; or $t2:expr) => (
        match $self.peak() {
            Some($cond) => {
                $self.next();
                Ok($t1)
            }
            _ => Ok($t2)
        }
    );
);

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        let mut o = Lexer {
            chars: src.chars(),
            pos: SrcPos { line: 0, col: 0 },
            next_pos: SrcPos { line: 0, col: 0 },
            current: None,
        };
        o.next();
        o
    }

    /// Returns the next available token, or an error.
    pub fn next_token(&mut self) -> Result<SrcToken, LexError> {
        match self.next() {
            Some('\n') => {
                self.next_pos.line += 1;
                self.next_pos.col = 0;
                self.next_token()
            },
            Some('\r') => {
                self.next_token()
            },
            Some('\t') => {
                self.next_token()
            },
            Some(' ') => {
                self.next_token()
            },
            Some('+') => Ok(Token::Punct(Punct::Plus).localize(self.pos)),
            Some('-') => {
                expect_char!(self; is '>' => Token::Punct(Punct::RightArrow).localize(self.pos); or Token::Punct(Punct::Minus).localize(self.pos))
            },
            Some('*') => Ok(Token::Punct(Punct::Astrix).localize(self.pos)),
            Some('/') => {
                match self.peak() {
                    Some('/') => {
                        self.next();
                        self.parse_comment()
                    },
                    Some('*') => {
                        self.next();
                        self.parse_block_comment()
                    },
                    _ => Ok(Token::Punct(Punct::Slash).localize(self.pos))
                }
            },
            Some('&') => {
                let pos = self.pos;
                expect_char!(self; is '&' => Token::Punct(Punct::LAnd).localize(pos); or Token::Punct(Punct::And).localize(pos))
            }
            Some('|') => {
                let pos = self.pos;
                expect_char!(self; is '|' => Token::Punct(Punct::LOr).localize(pos); or Token::Punct(Punct::Or).localize(pos))
            }
            Some('^') => {
                let pos = self.pos;
                expect_char!(self; is '^' => Token::Punct(Punct::LXor).localize(pos); or Token::Punct(Punct::Xor).localize(pos))
            }
            Some('!') => {
                let pos = self.pos;
                expect_char!(self; is '=' => Token::Punct(Punct::Neq).localize(pos); or Token::Punct(Punct::Not).localize(pos))
            }
            Some('=') => {
                let pos = self.pos;
                Ok(match self.peak() {
                    Some('=') => {
                        self.next();
                        Token::Punct(Punct::Eq).localize(pos)
                    },
                    Some('>') => {
                        self.next();
                        Token::Punct(Punct::BigRightArrow).localize(pos)
                    },
                    _ => Token::Punct(Punct::Assign).localize(pos)
                })
            }
            Some('<') => {
                let pos = self.pos;
                match self.peak() {
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::Leq).localize(pos))
                    }
                    Some('<') => {
                        self.next();
                        Ok(Token::Punct(Punct::Lst).localize(pos))
                    },
                    _ => Ok(Token::Punct(Punct::Lt).localize(pos))
                }
            }
            Some('>') => {
                let pos = self.pos;
                match self.peak() {
                    Some('=') => {
                        self.next();
                        Ok(Token::Punct(Punct::Geq).localize(pos))
                    }
                    Some('>') => {
                        self.next();
                        Ok(Token::Punct(Punct::Rst).localize(pos))
                    }
                    _ => Ok(Token::Punct(Punct::Gt).localize(pos))
                }
            }
            Some(':') => {
                let pos = self.pos;
                expect_char!(self; is ':' => Token::Punct(Punct::DoubleColon).localize(pos); or Token::Punct(Punct::Colon).localize(pos))
            }
            Some(';') => Ok(Token::Punct(Punct::Semicolon).localize(self.pos)),
            Some(',') => Ok(Token::Punct(Punct::Comma).localize(self.pos)),
            Some('.') => Ok(Token::Punct(Punct::Dot).localize(self.pos)),
            Some('(') => Ok(Token::Punct(Punct::BracketOpen).localize(self.pos)),
            Some(')') => Ok(Token::Punct(Punct::BracketClose).localize(self.pos)),
            Some('{') => Ok(Token::Punct(Punct::BraceOpen).localize(self.pos)),
            Some('}') => Ok(Token::Punct(Punct::BraceClose).localize(self.pos)),
            Some('[') => Ok(Token::Punct(Punct::SquareOpen).localize(self.pos)),
            Some(']') => Ok(Token::Punct(Punct::SquareClose).localize(self.pos)),
            Some('%') => Ok(Token::Punct(Punct::Rem).localize(self.pos)),

            Some('0') => self.parse_number(0),
            Some('1') => self.parse_number(1),
            Some('2') => self.parse_number(2),
            Some('3') => self.parse_number(3),
            Some('4') => self.parse_number(4),
            Some('5') => self.parse_number(5),
            Some('6') => self.parse_number(6),
            Some('7') => self.parse_number(7),
            Some('8') => self.parse_number(8),
            Some('9') => self.parse_number(9),

            Some('"') => self.parse_string(),
            Some('\'') => self.parse_char(),

            Some(s) => self.parse_ident(s),
            _ => Err(LexError::EndOfStream(self.pos)),
        }
    }

    fn parse_integer(&mut self, mut x: usize) -> NumWithLeading {
        let mut leading = 0;
        loop {
            match self.peak() {
                Some('0') => {
                    if x == 0 {
                        leading += 1;
                    }
                    x *= 10;
                    self.next();
                },
                Some('1') => {
                    x = x * 10 + 1;
                    self.next();
                },
                Some('2') => {
                    x = x * 10 + 2;
                    self.next();
                },
                Some('3') => {
                    x = x * 10 + 3;
                    self.next();
                },
                Some('4') => {
                    x = x * 10 + 4;
                    self.next();
                },
                Some('5') => {
                    x = x * 10 + 5;
                    self.next();
                },
                Some('6') => {
                    x = x * 10 + 6;
                    self.next();
                },
                Some('7') => {
                    x = x * 10 + 7;
                    self.next();
                },
                Some('8') => {
                    x = x * 10 + 8;
                    self.next();
                },
                Some('9') => {
                    x = x * 10 + 9;
                    self.next();
                },
                Some('_') => {
                    self.next();
                }
                _ => break NumWithLeading(x, leading)
            }
        }
    }

    fn parse_number(&mut self, first: usize) -> Result<SrcToken, LexError> {
        // parse first item
        let pos = self.pos;
        let x = self.parse_integer(first).0;

        // parse second item
        let y = if self.peak().cloned() == Some('.') {
            self.next();
            Some(self.parse_integer(0))
        } else {
            None
        };

        // parse third item
        let z = if self.peak().cloned() == Some('e') {
            self.next();
            match self.peak() {
                Some('+') => {
                    self.next();
                    Some(self.parse_integer(0).0 as isize)
                },
                Some('-') => {
                    self.next();
                    let i = self.parse_integer(0).0 as isize;
                    Some(-i)
                },
                Some('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '_') => {
                    Some(self.parse_integer(0).0 as isize)
                },
                Some(c) => {
                    return if let Some(y) = y {
                        Err(LexError::InvalidNumber(format!("{x}.{y}e{c}"), self.pos))
                    } else {
                        Err(LexError::InvalidNumber(format!("{x}e{c}"), self.pos))
                    }
                },
                None => {
                    return Err(LexError::EndOfStream(self.pos));
                }
            }
        } else {
            None
        };

        // parse type hint
        let hint = match self.peak() {
            Some('f' | 'i' | 'u') => {
                let c = self.next().unwrap();
                Some(self.parse_ident_str(c))
            },
            _ => None,
        };
        Ok(Token::NumLiteral(x, y, z, hint).localize(pos))
    }

    fn parse_comment(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let pos = self.pos;
        loop {
            match self.next() {
                Some('\n') | None => break Ok(Token::Comment(s).localize(pos)),
                Some(c) => s.push(c)
            }
        }
    }

    fn parse_block_comment(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let pos = self.pos;
        loop {
            match self.next() {
                Some('*') => {
                    match self.peak() {
                        Some('/') => {
                            self.next();
                            break Ok(Token::Comment(s).localize(pos));
                        }
                        _ => {
                            s.push('*');
                        }
                    }
                },
                Some(c) => s.push(c),
                None => break Err(LexError::EndOfStream(self.pos))
            }
        }
    }

    fn parse_string(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let pos = self.pos;
        loop {
            match self.next() {
                Some('\\') => {
                    match self.peak() {
                        Some('"') => {
                            self.next();
                            s.push('"');
                        }
                        _ => s.push('\\')
                    }
                }
                Some('"') | None => break Ok(Token::StrLiteral(Self::clean_escape_sequences(s)).localize(pos)),
                Some(c) => s.push(c),
            }
        }
    }

    fn parse_char(&mut self) -> Result<SrcToken, LexError> {
        let mut s = "".to_owned();
        let pos = self.pos;
        loop {
            match self.next() {
                Some('\\') => {
                    match self.peak() {
                        Some('\'') => {
                            self.next();
                            s.push('\'');
                        }
                        _ => s.push('\\')
                    }
                }
                Some('\'') | None => break Ok(Token::CharLiteral(self.build_char(s)?).localize(pos)),
                Some(c) => s.push(c),
            }
        }
    }

    fn build_char(&self, input: String) -> Result<char, LexError> {
        if input.chars().count() == 1 {
            input.chars().next().ok_or(LexError::InvalidChar(input, self.pos))
        } else if input == "\\" {
            Ok('\\')
        } else if input == "\r" {
            Ok('\r')
        } else if input == "\n" {
            Ok('\n')
        } else if input == "\t" {
            Ok('\t')
        } else if input == "\"" {
            Ok('"')
        } else if input == "\'" {
            Ok('\'')
        } else {
            Err(LexError::InvalidChar(input, self.pos))
        }
    }

    fn clean_escape_sequences(input: String) -> String {
        input.replace("\\\\", "\\")
            .replace("\\r", "\r")
            .replace("\\n", "\n")
            .replace("\\t", "\t")
            .replace("\\\"", "\"")
            .replace("\\'", "'")
    }

    fn parse_ident_str(&mut self, first: char) -> String {
        let mut s = "".to_owned();
        s.push(first);

        loop {
            match self.peak() {
                Some(' ' | '\n' | '\r' | '\t'
                     | '+' | '-' | '*' | '/' | '&' | '|' | '^' | '!' | '=' | '<' | '>'
                     | ',' | ':' | '.' | '(' | ')' | '{' | '}' | '[' | ']' | ';') => {
                    break s
                }
                Some(_) => {
                    s.push(self.next().unwrap());
                }
                None => {
                    self.next();
                    break s
                }
            }
        }
    }

    fn parse_ident(&mut self, first: char) -> Result<SrcToken, LexError> {
        let pos = self.pos;
        Self::process_ident(self.parse_ident_str(first), pos)
    }

    fn process_ident(s: String, pos: SrcPos) -> Result<SrcToken, LexError> {
        match s {
            s if s == "fn" => Ok(Token::Key(KeyWord::Fn).localize(pos)),
            s if s == "for" => Ok(Token::Key(KeyWord::For).localize(pos)),
            s if s == "struct" => Ok(Token::Key(KeyWord::Struct).localize(pos)),
            s if s == "enum" => Ok(Token::Key(KeyWord::Enum).localize(pos)),
            s if s == "const" => Ok(Token::Key(KeyWord::Const).localize(pos)),
            s if s == "let" => Ok(Token::Key(KeyWord::Let).localize(pos)),
            s if s == "switch" => Ok(Token::Key(KeyWord::Switch).localize(pos)),
            s if s == "if" => Ok(Token::Key(KeyWord::If).localize(pos)),
            s if s == "else" => Ok(Token::Key(KeyWord::Else).localize(pos)),
            s if s == "loop" => Ok(Token::Key(KeyWord::Loop).localize(pos)),
            s if s == "return" => Ok(Token::Key(KeyWord::Return).localize(pos)),
            s if s == "break" => Ok(Token::Key(KeyWord::Break).localize(pos)),
            s if s == "continue" => Ok(Token::Key(KeyWord::Continue).localize(pos)),
            s if s == "impl" => Ok(Token::Key(KeyWord::Impl).localize(pos)),
            s if s == "trait" => Ok(Token::Key(KeyWord::Trait).localize(pos)),
            s if s == "self" => Ok(Token::Key(KeyWord::VarSelf).localize(pos)),
            s if s == "Self" => Ok(Token::Key(KeyWord::TySelf).localize(pos)),
            s if s == "mut" => Ok(Token::Key(KeyWord::Mut).localize(pos)),
            s if s == "as" => Ok(Token::Key(KeyWord::As).localize(pos)),
            s if s == "where" => Ok(Token::Key(KeyWord::Where).localize(pos)),
            s if s == "type" => Ok(Token::Key(KeyWord::Type).localize(pos)),
            s => Ok(Token::Ident(s).localize(pos)),
        }
    }

    fn peak(&self) -> Option<&char> {
        self.current.as_ref()
    }

    /// Returns the next symbol in the char stream.
    fn next(&mut self) -> Option<char> {
        let mut n = self.chars.next();
        mem::swap(&mut self.current, &mut n);

        self.pos = self.next_pos;
        match n {
            Some('\t') => {
                self.next_pos.col += 4;
            }
            Some('\r' | '\n') => (),
            Some(_) => {
                self.next_pos.col += 1;
            }
            _ => ()
        }
        n
    }
}



#[cfg(test)]
mod test {
    use crate::lexer::{KeyWord, Lexer, NumWithLeading, Punct, Token};

    #[test]
    fn test_single_token() {
        assert_eq!(Lexer::new("fn").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Fn)));
        assert_eq!(Lexer::new("struct").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Struct)));
        assert_eq!(Lexer::new("const").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Const)));
        assert_eq!(Lexer::new("let").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Let)));
        assert_eq!(Lexer::new("switch").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Switch)));
        assert_eq!(Lexer::new("if").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::If)));
        assert_eq!(Lexer::new("loop").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Loop)));
        assert_eq!(Lexer::new("return").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Return)));
        assert_eq!(Lexer::new("break").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Break)));
        assert_eq!(Lexer::new("impl").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Impl)));
        assert_eq!(Lexer::new("trait").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Trait)));
        assert_eq!(Lexer::new("enum").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Enum)));
        assert_eq!(Lexer::new("self").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::VarSelf)));
        assert_eq!(Lexer::new("Self").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::TySelf)));
        assert_eq!(Lexer::new("mut").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Mut)));
        assert_eq!(Lexer::new("where").next_token().map(|t| t.token), Ok(Token::Key(KeyWord::Where)));

        assert_eq!(Lexer::new("+").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Plus)));
        assert_eq!(Lexer::new("-").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Minus)));
        assert_eq!(Lexer::new("*").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Astrix)));
        assert_eq!(Lexer::new("/").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Slash)));

        assert_eq!(Lexer::new("&").next_token().map(|t| t.token), Ok(Token::Punct(Punct::And)));
        assert_eq!(Lexer::new("|").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Or)));
        assert_eq!(Lexer::new("^").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Xor)));

        assert_eq!(Lexer::new("&&").next_token().map(|t| t.token), Ok(Token::Punct(Punct::LAnd)));
        assert_eq!(Lexer::new("||").next_token().map(|t| t.token), Ok(Token::Punct(Punct::LOr)));
        assert_eq!(Lexer::new("^^").next_token().map(|t| t.token), Ok(Token::Punct(Punct::LXor)));

        assert_eq!(Lexer::new("!").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Not)));
        assert_eq!(Lexer::new("=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Assign)));

        assert_eq!(Lexer::new("==").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Eq)));
        assert_eq!(Lexer::new("!=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Neq)));
        assert_eq!(Lexer::new("<").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Lt)));
        assert_eq!(Lexer::new(">").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Gt)));
        assert_eq!(Lexer::new("<=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Leq)));
        assert_eq!(Lexer::new(">=").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Geq)));

        assert_eq!(Lexer::new(",").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Comma)));
        assert_eq!(Lexer::new(":").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Colon)));
        assert_eq!(Lexer::new(";").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Semicolon)));
        assert_eq!(Lexer::new("::").next_token().map(|t| t.token), Ok(Token::Punct(Punct::DoubleColon)));
        assert_eq!(Lexer::new(".").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Dot)));
        assert_eq!(Lexer::new("(").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BracketOpen)));
        assert_eq!(Lexer::new(")").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BracketClose)));
        assert_eq!(Lexer::new("{").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BraceOpen)));
        assert_eq!(Lexer::new("}").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BraceClose)));
        assert_eq!(Lexer::new("[").next_token().map(|t| t.token), Ok(Token::Punct(Punct::SquareOpen)));
        assert_eq!(Lexer::new("]").next_token().map(|t| t.token), Ok(Token::Punct(Punct::SquareClose)));

        assert_eq!(Lexer::new("<<").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Lst)));
        assert_eq!(Lexer::new(">>").next_token().map(|t| t.token), Ok(Token::Punct(Punct::Rst)));

        assert_eq!(Lexer::new("->").next_token().map(|t| t.token), Ok(Token::Punct(Punct::RightArrow)));
        assert_eq!(Lexer::new("=>").next_token().map(|t| t.token), Ok(Token::Punct(Punct::BigRightArrow)));
        assert_eq!(Lexer::new("// This is a comment\n").next_token().map(|t| t.token),
                   Ok(Token::Comment(" This is a comment".to_owned())));
        assert_eq!(Lexer::new("// This is a comment").next_token().map(|t| t.token),
                   Ok(Token::Comment(" This is a comment".to_owned())));
        assert_eq!(Lexer::new("/* This is a block comment */").next_token().map(|t| t.token),
                   Ok(Token::Comment(" This is a block comment ".to_owned())));

        assert_eq!(Lexer::new("\"Hello, World!\"").next_token().map(|t| t.token),
                   Ok(Token::StrLiteral("Hello, World!".to_owned())));
        assert_eq!(Lexer::new("'🦦'").next_token().map(|t| t.token), Ok(Token::CharLiteral('🦦')));
        assert_eq!(Lexer::new("usize").next_token().map(|t| t.token), Ok(Token::Ident("usize".to_owned())));
        assert_eq!(Lexer::new("🦦").next_token().map(|t| t.token), Ok(Token::Ident("🦦".to_owned())));
    }

    #[test]
    fn test_number_literal() {
        assert_eq!(Lexer::new("31_415").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       31415,
                       None,
                       None,
                       None
                   )));
        assert_eq!(Lexer::new("31415_usize").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       31415,
                       None,
                       None,
                       Some("usize".to_owned())
                   )));
        assert_eq!(Lexer::new("3.14_15").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       3,
                       Some(NumWithLeading(1415, 0)),
                       None,
                       None
                   )));
        assert_eq!(Lexer::new("3.14_15f32").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       3,
                       Some(NumWithLeading(1415, 0)),
                       None,
                       Some("f32".to_owned())
                   )));
        assert_eq!(Lexer::new("31415e-4__f64").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       31415,
                       None,
                       Some(-4),
                       Some("f64".to_owned())
                   )));
        assert_eq!(Lexer::new("314.15e-2").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       314,
                       Some(NumWithLeading(15, 0)),
                       Some(-2),
                       None
                   )));
        assert_eq!(Lexer::new("0.031415e2").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       0,
                       Some(NumWithLeading(31415, 1)),
                       Some(2),
                       None
                   )));
        assert_eq!(Lexer::new("0.031415e+2").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       0,
                       Some(NumWithLeading(31415, 1)),
                       Some(2),
                       None
                   )));
        assert_eq!(Lexer::new("314.15e-2f32").next_token().map(|t| t.token),
                   Ok(Token::NumLiteral(
                       314,
                       Some(NumWithLeading(15, 0)),
                       Some(-2),
                       Some("f32".to_owned())
                   )));
    }
}
