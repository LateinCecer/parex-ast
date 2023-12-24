use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};
use crate::parser::ast::ast_type::AstType;

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
    parameters: Vec<String>,
    constraints: Vec<Constraint>,
    consts: Vec<GenericConst>
}

pub struct GenericIdentifier {
    name: String,
    pos: SrcPos,
}

#[derive(Debug, Clone)]
pub struct GenericConst {
    name: String,
    ty: AstType,
    pos: SrcPos,
}



#[derive(Debug, Clone)]
/// A constraint is specified by a `where`-clause in the head of a parameter environment.
/// It constraints what the generic parameter can be.
/// This hints to the compiler that generic types can fulfill *obligations* during trait resolution.
pub struct Constraint {
    /// The constrainee must be a generic parameter, or a type that is constructed from a generic
    /// parameter.
    /// The point is, that only generic parameters can be constraint.
    constrainee: AstType,
    /// The constraint must point towards a trait in scope.
    constraint: AstType,
    pos: SrcPos,
}

impl Parsable for Constraint {
    type Output = Vec<Self>;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let constrainee = AstType::parse(parser)?;
        let pos = expect_token!(parser; (Token::Punct(Punct::Colon)), pos => pos
            expected "list of type constraints")?;

        // parse list of constraints
        let mut constraints = Vec::new();
        loop {
            let constraint = AstType::parse(parser)?;
            constraints.push(Constraint {
                constrainee: constrainee.clone(),
                constraint,
                pos,
            });

            if let Ok(local!(Token::Punct(Punct::Plus))) = parser.peak() {
                parser.next()?;
            } else {
                break
            }
        }
        Ok(constraints)
    }
}

impl Display for Constraint {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.constrainee, self.constraint)
    }
}


impl ParameterEnv {
    pub fn parse_where(&mut self, parser: &mut Parser) -> Result<(), ParseError> {
        expect_token!(parser; (Token::Key(KeyWord::Where)) expected "`where` keyword")?;
        loop {
            if let Ok(local!(Token::Punct(Punct::BraceOpen | Punct::Semicolon))) = parser.peak() {
                break
            }
            // parser constraint
            let mut constraints = Constraint::parse(parser)?;
            self.constraints.append(&mut constraints);
            // check for comma
            if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                parser.next()?;
            } else {
                break;
            }
        }
        Ok(())
    }

    pub fn fmt_where(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        if self.constraints.is_empty() {
           return Ok(());
        }
        writeln!(fmt, "where")?;

        for constraint in self.constraints.iter() {
            writeln!(fmt, "    {constraint},")?;
        }
        Ok(())
    }
}

impl Display for ParameterEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // write!(f, "<>")?;
        if !self.consts.is_empty() || !self.parameters.is_empty() {
            write!(f, "<")?;
            // write parameters
            let mut is_first = true;
            for param in self.parameters.iter() {
                if !is_first {
                    write!(f, ", {param}")?;
                } else {
                    is_first = false;
                    write!(f, "{param}")?;
                }
            }

            // write consts
            for param in self.consts.iter() {
                if !is_first {
                    write!(f, ", {param}")?;
                } else {
                    is_first = false;
                    write!(f, "{param}")?;
                }
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl Display for GenericConst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "const {}: {}", self.name, self.ty)
    }
}


impl ParameterEnv {
    fn parse_parameter_def(parser: &mut Parser) -> Result<String, ParseError> {
        expect_token!(parser; (Token::Ident(name)) => name
            expected "generic parameter name identifier")
    }
}

impl Parsable for GenericConst {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Const)), pos => pos
            expected "`const` generic identifier starting with `const` keyword")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "`const` generic name identifier")?;
        expect_token!(parser; (Token::Punct(Punct::Colon))
            expected "`const` generic identifier type starting with `:`")?;
        let ty = AstType::parse(parser)?;

        Ok(GenericConst {
            name,
            ty,
            pos,
        })
    }
}


impl Parsable for ParameterEnv {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let mut parameters = Vec::new();
        let mut consts = Vec::new();

        if let Ok(local!(Token::Punct(Punct::Lt))) = parser.peak() {
            parser.next()?;
            // parse generic parameters first, then consts
            loop {
                match parser.peak() {
                    Ok(local!(Token::Key(KeyWord::Const))) => break,
                    Ok(local!(Token::Punct(Punct::Gt))) => break,
                    Ok(local!(Token::Ident(_))) => (),
                    Ok(_) => return Err(ParseError::UnexpectedToken(
                        Box::new(parser.next().unwrap()),
                        "expected generic name identifier, `const` generic identifier or `>`"
                            .to_string(),
                    )),
                    Err(_) => return Err(ParseError::LexError(parser.next().err().unwrap())),
                }
                // parse parameter
                let param = Self::parse_parameter_def(parser)?;
                parameters.push(param);
                // check for comma or closing `>`
                if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                    parser.next()?;
                } else {
                    expect_token!(parser; (Token::Punct(Punct::Gt))
                        expected "`,` or `>` in parameter environment")?;
                }
            }

            // parse consts
            loop {
                match parser.peak() {
                    Ok(local!(Token::Key(KeyWord::Const))) => (),
                    Ok(local!(Token::Punct(Punct::Gt))) => break,
                    Ok(_) => return Err(ParseError::UnexpectedToken(
                        Box::new(parser.next().unwrap()),
                        "expected `const` generic identifier or `>`"
                            .to_string(),
                    )),
                    Err(_) => return Err(ParseError::LexError(parser.next().err().unwrap())),
                }
                // parse parameter
                let param = GenericConst::parse(parser)?;
                consts.push(param);
                // check for comma or closing `>`
                if let Ok(local!(Token::Punct(Punct::Comma))) = parser.peak() {
                    parser.next()?;
                } else {
                    expect_token!(parser; (Token::Punct(Punct::Gt))
                        expected "`,` or `>` in parameter environment")?;
                }
            }
        }

        Ok(ParameterEnv {
            parameters,
            constraints: Vec::new(),
            consts,
        })
    }
}


#[cfg(test)]
mod test {

    #[test]
    fn test_condition() {

    }
}
