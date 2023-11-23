use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, Token};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::array_index::AstArrayIndex;
use crate::parser::ast::array_init::AstArrayInit;
use crate::parser::ast::as_expr::AsExpr;
use crate::parser::ast::ast_type::AstType;
use crate::parser::ast::binary::AstBinaryExpr;
use crate::parser::ast::block::AstBlock;
use crate::parser::ast::break_expr::AstBreak;
use crate::parser::ast::call::{AstCall, AstInit, AstMethodCall};
use crate::parser::ast::continue_expr::AstContinue;
use crate::parser::ast::field::AstFieldExpr;
use crate::parser::ast::if_expr::AstIf;
use crate::parser::ast::let_expr::AstLet;
use crate::parser::ast::literal::{AstBoolLiteral, AstCharLiteral, AstNumberLiteral, AstStringLiteral};
use crate::parser::ast::loop_expr::AstLoop;
use crate::parser::ast::name::AstNameExpr;
use crate::parser::ast::prefix_operator::{AstPrefixExpr};
use crate::parser::ast::return_expr::AstReturn;
use crate::parser::ast::switch_expr::AstSwitch;
use crate::parser::r#type::Type;


#[derive(Debug, Clone, PartialEq)]
pub enum AstExpression {
    StrLit(AstStringLiteral),
    CharLit(AstCharLiteral),
    NumberLit(AstNumberLiteral),
    BoolLit(AstBoolLiteral),
    ArrayInit(AstArrayInit),
    ArrayIndex(AstArrayIndex),
    Binary(AstBinaryExpr),
    Init(AstInit),
    FunctionCall(AstCall),
    MethodCall(AstMethodCall),
    Name(AstNameExpr),
    Field(AstFieldExpr),
    Prefix(AstPrefixExpr),
    Block(AstBlock),
    If(AstIf),
    Switch(AstSwitch),
    Loop(AstLoop),
    Return(AstReturn),
    Break(AstBreak),
    Continue(AstContinue),
    As(AsExpr),
    Let(AstLet),
}


pub struct StaticValue {
    ty: Type,
    data: Vec<u8>,
}

impl StaticValue {
    pub fn as_u64(self) -> Option<u64> {
        todo!()
    }
}

impl AstExpression {
    /// Returns `true` if the expression must be terminated by a `;` if used as an expression
    /// statement.
    pub fn must_terminate_statement(&self) -> bool {
        !matches!(self, AstExpression::Switch(_) | AstExpression::If(_)
            | AstExpression::Block(_) | AstExpression::Loop(_))
    }

    /// Evaluate static value.
    pub fn static_eval(&self, parser: &mut Parser) -> Option<StaticValue> {
        None
    }

    pub fn parse_primary(parser: &mut Parser) -> Result<Self, ParseError> {
        // check for prefix operator
        if let Ok(local!(Token::Punct(
            Punct::And | Punct::Astrix | Punct::Not | Punct::Minus
        ))) = parser.peak() {
            return AstPrefixExpr::parse(parser)
        }

        // check main
        let mut x = match parser.peak() {
            Ok(local!(Token::NumLiteral(_, _, _, _))) => AstNumberLiteral::parse(parser),
            Ok(local!(Token::StrLiteral(_))) => AstStringLiteral::parse(parser),
            Ok(local!(Token::CharLiteral(_))) => AstCharLiteral::parse(parser),
            Ok(local!(Token::Punct(Punct::SquareOpen))) => AstArrayInit::parse(parser),
            Ok(local!(Token::Punct(Punct::BraceOpen))) => AstBlock::parse(parser).map(|block| block.into()),
            Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                parser.next()?;
                let x = AstExpression::parse(parser);
                expect_token!(parser; (Token::Punct(Punct::BracketClose)) expected ")")?;
                x
            },
            Ok(local!(Token::Key(KeyWord::If))) => AstIf::parse(parser).map(|case| case.into()),
            Ok(local!(Token::Key(KeyWord::Switch))) => AstSwitch::parse(parser).map(|case| case.into()),
            Ok(local!(Token::Key(KeyWord::Loop))) => AstLoop::parse(parser).map(|lo| lo.into()),
            Ok(local!(Token::Key(KeyWord::Return))) => AstReturn::parse(parser),
            Ok(local!(Token::Key(KeyWord::Break))) => AstBreak::parse(parser),
            Ok(local!(Token::Key(KeyWord::Continue))) => AstContinue::parse(parser),
            Ok(local!(Token::Key(KeyWord::Let))) => AstLet::parse(parser).map(|l| l.into()),
            Ok(local!(Token::Ident(ident)))
                if ident == "false" || ident == "true" => AstBoolLiteral::parse(parser),
            Ok(local!(Token::Ident(_))) => {
                let x = AstNameExpr::parse(parser)?;
                if parser.env.find_struct(&x).is_some() {
                    match parser.peak() {
                        Ok(local!(Token::Punct(Punct::BraceOpen | Punct::BracketOpen))) => {
                            AstInit::parse_lhs(parser, x.into())
                        },
                        _ => Ok(x.into())
                    }
                } else {
                    Ok(x.into())
                }
            },
            Ok(local!(Token::Key(KeyWord::VarSelf))) => {
                parser.next()?;
                Ok(AstNameExpr::var("self".to_owned()))
            }
            Ok(_) => Err(ParseError::UnexpectedToken(
                Box::new(parser.next().unwrap()),
                "primary expression".to_owned())),
            Err(e) => Err(ParseError::LexError(e.clone())),
        }?;

        // check fields, method calls, index operator
        loop {
            match parser.peak() {
                Ok(local!(Token::Punct(Punct::BracketOpen))) => {
                    x = AstCall::parse_lhs(parser, Box::new(x))?;
                },
                Ok(local!(Token::Punct(Punct::Dot))) => {
                    parser.next()?;
                    let field = expect_token!(parser; (Token::Ident(name)) => { name.clone() }
                        expected "field identifier")?;

                    // look for subsequent bracket opening for
                    if let Ok(local!(Token::Punct(Punct::BracketOpen))) = parser.peak() {
                        x = AstMethodCall::parse_lhs(parser, Box::new(x), field)?;
                    } else {
                        x = AstFieldExpr::new(Box::new(x), field).into();
                    }
                },
                Ok(local!(Token::Punct(Punct::SquareOpen))) => {
                    x = AstArrayIndex::parse_lhs(parser, Box::new(x))?;
                }
                _ => break,
            }
        }

        // check for as expr
        if let Ok(local!(Token::Key(KeyWord::As))) = parser.peak() {
            x = AsExpr::parse_lhs(parser, Box::new(x))?.into();
        }

        Ok(x)
    }
}

impl Parsable for AstExpression {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        AstBinaryExpr::parse(parser)
    }
}

impl AstTyped for AstExpression {
    fn ast_type(&self) -> Option<AstType> {
        todo!()
    }
}

pub trait AstTyped {
    /// Returns the type of the Ast Expression. If the expression does not have a type fixture
    /// yet, this method return `None`.
    fn ast_type(&self) -> Option<AstType>;
}

pub trait AstTypedMut: AstTyped {
    /// Sets the type fixture for this ast expression.
    fn suggest(&mut self, ty: AstType) -> bool;
}

impl Display for AstExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AstExpression::StrLit(lit) => write!(f, "{}", lit),
            AstExpression::CharLit(lit) => write!(f, "{}", lit),
            AstExpression::NumberLit(lit) => write!(f, "{}", lit),
            AstExpression::BoolLit(lit) => write!(f, "{}", lit),
            AstExpression::ArrayInit(expr) => write!(f, "{}", expr),
            AstExpression::ArrayIndex(expr) => write!(f, "{}", expr),
            AstExpression::Binary(expr) => write!(f, "{}", expr),
            AstExpression::Init(expr) => write!(f, "{}", expr),
            AstExpression::MethodCall(expr) => write!(f, "{}", expr),
            AstExpression::Name(expr) => write!(f, "{}", expr),
            AstExpression::Field(expr) => write!(f, "{}", expr),
            AstExpression::Prefix(expr) => write!(f, "{}", expr),
            AstExpression::Block(expr) => write!(f, "{}", expr),
            AstExpression::If(expr) => write!(f, "{}", expr),
            AstExpression::Switch(expr) => write!(f, "{}", expr),
            AstExpression::Loop(expr) => write!(f, "{}", expr),
            AstExpression::Return(expr) => write!(f, "{}", expr),
            AstExpression::Break(expr) => write!(f, "{}", expr),
            AstExpression::Continue(expr) => write!(f, "{}", expr),
            AstExpression::As(expr) => write!(f, "{}", expr),
            AstExpression::Let(expr) => write!(f, "{}", expr),
            AstExpression::FunctionCall(expr) => write!(f, "{}", expr),
        }
    }
}
