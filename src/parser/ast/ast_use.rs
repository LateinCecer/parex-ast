use std::fmt::{Display, Formatter};
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::ast_type::QualifierName;
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser};


#[derive(Clone, Debug)]
pub struct AstUse {
    pub path: QualifierName,
    pos: SrcPos,
}

impl Parsable for AstUse {
    type Output = AstUse;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Use)), pos => pos
            expected "`use`-statement starting starting with the keyword `use`")?;

        let mut elements = Vec::new();
        loop {
            let element = expect_token!(parser; (Token::Ident(name)) => { name.clone() }
                expected "entry element of qualifier name in `use`-statement")?;
            elements.push(element);

            // check if other elements can be parsed
            if let Ok(local!(Token::Punct(Punct::DoubleColon))) = parser.peak() {
                parser.next()?;
            } else {
                break;
            }
        }
        // check if the use statement is terminated with a semicolon
        expect_token!(parser; (Token::Punct(Punct::Semicolon))
            expected "semicolon `;` after `use`-statement")?;
        Ok(AstUse {
            path: QualifierName::from(elements),
            pos,
        })
    }
}

impl Display for AstUse {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "use {};", self.path)
    }
}
