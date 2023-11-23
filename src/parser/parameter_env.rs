use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Token};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, ParseError, Parser};
use crate::parser::r#type::LangTypeId;


#[derive(Copy, Clone, Debug, PartialOrd, PartialEq, Ord, Eq, Hash)]
pub struct ParameterId(usize, usize);

impl Display for ParameterId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "generic parameter <{}.{}>", self.0, self.1)
    }
}



#[derive(Debug, Clone)]
/// A parameter environment defines generic parameters within a certain scope.
/// These parameters generally have a name identifier and a number of constraints, specified by
/// `where`-clauses
pub struct ParameterEnv {
    parameters: HashMap<String, ParameterId>,
    constraints: Vec<Constraint>,
}



#[derive(Debug, Clone)]
/// A constraint is specified by a `where`-clause in the head of a parameter environment.
/// It constraints what the generic parameter can be.
/// This hints to the compiler that generic types can fulfill *obligations* during trait resolution.
pub struct Constraint {
    /// The constrainee must be a generic parameter, or a type that is constructed from a generic
    /// parameter.
    /// The point is, that only generic parameters can be constraint.
    constrainee: ParameterId,
    /// The constraint must point towards a trait in scope.
    constraint: LangTypeId,
}


impl ParameterEnv {
    pub fn parse_where(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::Where)) expected "`where` keyword")?;
        // todo implement this
        Ok(())
    }

    pub fn fmt_where(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        // write!(fmt, "where ")?;
        // todo implement this
        Ok(())
    }
}

impl Display for ParameterEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // write!(f, "<>")?;
        Ok(())
    }
}


impl Parsable for ParameterEnv {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        Ok(ParameterEnv {
            parameters: HashMap::new(),
            constraints: Vec::new(),
        })
    }
}
