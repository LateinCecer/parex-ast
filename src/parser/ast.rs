use crate::parser::{ParseError, Parser};

pub mod expression;
pub mod statement;
mod block;
mod binary;
mod array_index;
mod array_init;
pub mod ast_type;
mod prefix_operator;
mod literal;
mod field;
mod call;
mod name;
mod if_expr;
mod switch_expr;
mod return_expr;
mod break_expr;
mod continue_expr;
mod loop_expr;
mod as_expr;
mod let_expr;
mod element;
pub mod ast_func;
pub mod ast_struct;
pub  mod ast_impl;
pub mod ast_trait;
pub mod ast_const;
pub mod ast_use;
pub mod ast_type_def;
mod ast_pattern_matching;


pub struct Ast {

}


pub trait Parsable {
    type Output;
    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError>;
}
