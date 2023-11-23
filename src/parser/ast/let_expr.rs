use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::expression::AstExpression;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};

#[derive(Clone, Debug, PartialEq)]
pub struct AstLet {
    pos: SrcPos,
    name: String,
    mutable: bool,
    ty: Option<AstType>,
    value: Option<Box<AstExpression>>,
}

impl Parsable for AstLet {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Let)), pos => pos
            expected "let statement")?;

        // check for mutability
        let mutable = if let Ok(local!(Token::Key(KeyWord::Mut))) = parser.peak() {
            parser.next()?;
            true
        } else {
            false
        };

        // get name
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "variable name identifier")?;

        // check for explicit type
        let ty = if let Ok(local!(Token::Punct(Punct::Colon))) = parser.peak() {
            parser.next()?;
            Some(AstType::parse(parser)?)
        } else {
            None
        };

        // check for value
        let value = if let Ok(local!(Token::Punct(Punct::Assign))) = parser.peak() {
            parser.next()?;
            Some(Box::new(AstExpression::parse(parser)?))
        } else {
            None
        };

        Ok(AstLet {
            value,
            name,
            mutable,
            ty,
            pos,
        })
    }
}

impl From<AstLet> for AstExpression {
    fn from(value: AstLet) -> Self {
        AstExpression::Let(value)
    }
}

impl Display for AstLet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "let ")?;
        if self.mutable {
            write!(f, "mut ")?;
        }
        write!(f, "{}", self.name)?;
        if let Some(ty) = &self.ty {
            write!(f, ": {ty}")?;
        }
        if let Some(val) = &self.value {
            write!(f, " = {val}")?;
        }
        Ok(())
    }
}
