use crate::lexer;
use crate::lexer::{KeyWord, Punct, SrcPos, Token};
use crate::parser::ast::Parsable;
use crate::parser::{expect_token, local, ParseError, Parser, skip_to};
use crate::parser::ast::ast_func::AstFunc;
use crate::parser::ast::ast_impl::AstImpl;
use crate::parser::ast::ast_trait::AstTrait;
use crate::parser::ast::ast_type::{AstType, TypeName};
use crate::parser::ast::ast_type_def::AstTypeDef;
use crate::parser::ast::ast_use::AstUse;
use crate::parser::resolver::{ItemVariant, ScopeId};


#[derive(Clone, Debug)]
struct PreModule {
    name: Option<String>,
    pos: Option<SrcPos>,

    types: Vec<PreType>,
    impls: Vec<PreImpl>,
    traits: Vec<PreTrait>,
    funcs: Vec<PreFunc>,
    uses: Vec<AstUse>,
    stack_level: ScopeId,
}


#[derive(Clone, Debug)]
enum PreTypeVariant {
    Struct,
    Enum (Vec<String>),
    Alias,
}


#[derive(Clone, Debug)]
pub struct PreType {
    name: String,
    pos: SrcPos,
    var: PreTypeVariant,
}


#[derive(Clone, Debug)]
pub struct PreImpl {
    pos: SrcPos,
}


#[derive(Clone, Debug)]
pub struct PreTrait {
    name: String,
    pos: SrcPos,
}


#[derive(Clone, Debug)]
pub struct PreFunc {
    pos: SrcPos,
    name: String,
}


#[derive(Clone, Debug)]
pub struct PreConst {
    pos: SrcPos,
    name: String,
}



fn skip_type(parser: &mut Parser) -> Result<(), ParseError> {
    // ast types can be parsed largely independently from the environment, if parsed in a module
    // context (not inside an `impl`).
    AstType::parse(parser)?;
    Ok(())
}

fn skip_where(parser: &mut Parser) -> Result<(), ParseError> {
    if let Ok(local!(Token::Key(KeyWord::Where))) = parser.peak() {
        expect_token!(parser; (Token::Key(KeyWord::Where))
            expected "`where` clause starting with keyword `where`")?;

        todo!("implement parameter environments")
    }
    Ok(())
}


impl PreTypeVariant {
    fn skip_struct_body(parser: &mut Parser) -> Result<(), ParseError> {
        match parser.peak() {
            Ok(local!(Token::Punct(Punct::BraceOpen))) => parser.skip_brace(),
            Ok(local!(Token::Punct(Punct::BracketClose))) => parser.skip_bracket(),
            Ok(_) => Ok(()),
            Err(_) => Err(ParseError::LexError(parser.next().err().unwrap())),
        }
    }
}

impl Parsable for PreTypeVariant {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        match parser.next()? {
            local!(Token::Key(KeyWord::Struct)) => {
                // parse struct
                Self::skip_struct_body(parser)?;
                Ok(Self::Struct)
            },
            local!(Token::Key(KeyWord::Enum)) => {
                // parse enum
                expect_token!(parser; (Token::Punct(Punct::BraceOpen))
                    expected "enum variant list starting with `{`")?;

                let mut variants = Vec::new();
                loop {
                    match parser.peak() {
                        Ok(local!(Token::Punct(Punct::BraceClose))) => {
                            parser.next()?;
                            break;
                        },
                        Ok(local!(Token::Punct(Punct::Comma))) => {
                            parser.next()?;
                            continue;
                        },
                        Ok(_) => (),
                        Err(_) => {
                            return Err(ParseError::LexError(parser.next().err().unwrap()))
                        }
                    }

                    let name = expect_token!(parser; (Token::Ident(name)) => { name.clone() }
                        expected "enum variant")?;
                    Self::skip_struct_body(parser)?;
                    variants.push(name);
                    // check if there is a comma
                    match parser.peak() {
                        Ok(local!(Token::Punct(Punct::Comma))) => {
                            parser.next()?;
                        },
                        Ok(local!(Token::Punct(Punct::BraceClose))) => {
                            parser.next()?;
                            break;
                        },
                        Ok(_) => return Err(ParseError::UnexpectedToken(
                            Box::new(parser.next().unwrap()),
                            "expected `,` or `}` in enum definition".to_owned(),
                        )),
                        Err(_) => return Err(ParseError::LexError(
                            parser.next().err().unwrap()
                        )),
                    }
                }
                Ok(Self::Enum(variants))
            },
            _ => {
                // parse type alias
                skip_to!(parser, (Token::Punct(Punct::Semicolon)))?;
                parser.next()?;
                Ok(Self::Alias)
            }
        }
    }
}

impl Parsable for PreType {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Type)), pos => pos
            expected "type definition starting with `type` keyword")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "type name")?;

        // skip environment
        parser.skip_env()?;
        skip_where(parser)?;
        expect_token!(parser; (Token::Punct(Punct::Assign)) expected "type assignment `=`")?;

        // parse variant
        let var = PreTypeVariant::parse(parser)?;
        Ok(PreType {
            name,
            pos,
            var,
        })
    }
}


impl Parsable for PreFunc {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Fn)), pos => pos
            expected "`fn` keyword")?;
        parser.skip_env()?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "function name identifier")?;
        skip_to!(parser, (Token::Punct(Punct::BraceOpen)))?;

        parser.skip_brace()?;
        Ok(PreFunc {
            pos,
            name,
        })
    }
}

impl Parsable for PreImpl {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Impl)), pos => pos
            expected "`impl` keyword")?;

        skip_to!(parser, (Token::Punct(Punct::BraceOpen)))?;
        parser.skip_brace()?;
        Ok(PreImpl {
            pos
        })
    }
}

impl Parsable for PreTrait {
    type Output = Self;

    fn parse(parser: &mut Parser) -> Result<Self::Output, ParseError> {
        let pos = expect_token!(parser; (Token::Key(KeyWord::Trait)), pos => pos
            expected "`trait` keyword")?;
        let name = expect_token!(parser; (Token::Ident(name)) => name
            expected "trait name identifier")?;

        parser.skip_env()?;
        skip_to!(parser, (Token::Punct(Punct::BraceOpen)))?;

        parser.skip_brace()?;
        Ok(PreTrait {
            name, pos
        })
    }
}


impl PreModule {

    fn parse(parser: &mut Parser, name: String) -> Result<Self, ParseError> {
        parser.env.push_module(name);

        let mut types = Vec::new();
        let mut impls= Vec::new();
        let mut traits = Vec::new();
        let mut funcs = Vec::new();
        let mut uses = Vec::new();

        loop {
            match parser.peak() {
                Ok(local!(Token::Key(KeyWord::Type))) => {
                    types.push(PreType::parse(parser)?);
                },
                Ok(local!(Token::Key(KeyWord::Fn))) => {
                    funcs.push(PreFunc::parse(parser)?);
                },
                Ok(local!(Token::Key(KeyWord::Impl))) => {
                    impls.push(PreImpl::parse(parser)?);
                },
                Ok(local!(Token::Key(KeyWord::Trait))) => {
                    traits.push(PreTrait::parse(parser)?);
                },
                Ok(local!(Token::Key(KeyWord::Const))) => {
                    unimplemented!()
                },
                Ok(local!(Token::Key(KeyWord::Use))) => {
                    uses.push(AstUse::parse(parser)?);
                }
                Ok(_) => {
                    return Err(ParseError::UnexpectedToken(
                        Box::new(parser.next().unwrap()),
                        "expected item: `type`, `fn`, `const`, `impl` or `trait`".to_owned()));
                }
                Err(lexer::LexError::EndOfStream(_)) => break,
                Err(_) => return Err(ParseError::LexError(parser.next().err().unwrap())),
            }
        }

        // create module and add items into the namespace entry for it
        let module = PreModule {
            name: None, pos: None,
            types,
            impls,
            traits,
            funcs,
            uses,
            stack_level: parser.env.current_scope()?.clone(),
        };
        module.push_types(parser)?;
        module.push_traits(parser)?;
        module.push_use(parser)?;

        // pop out of the current stack level
        parser.env.pop();
        Ok(module)
    }
}

impl PreModule {
    /// Pushes types to the namespace structure of the parser
    pub fn push_types(&self, parser: &mut Parser) -> Result<(), ParseError> {
        // push structs
        for s in self.types.iter() {
            match &s.var {
                PreTypeVariant::Struct => {
                    parser.env.push_top_level_item(s.name.clone(), s.pos, ItemVariant::Type)?;
                }
                PreTypeVariant::Enum(vars) => {
                    let scope = parser.env.push_top_level_item(
                        s.name.clone(), s.pos, ItemVariant::Type)?;
                    parser.env.revert_to_scope(&scope);
                    for var in vars.iter() {
                        parser.env.push_top_level_item(
                            var.clone(),
                            s.pos,
                            ItemVariant::Type
                        )?;
                    }
                    parser.env.pop();
                }
                PreTypeVariant::Alias => {
                    parser.env.push_top_level_item(s.name.clone(), s.pos, ItemVariant::Type)?;
                }
            }
        }
        Ok(())
    }

    /// Pushes traits to the namespace structure of the parser.
    pub fn push_traits(&self, parser: &mut Parser) -> Result<(), ParseError> {
        // push traits
        for t in self.traits.iter() {
            parser.env.push_top_level_item(t.name.clone(), t.pos, ItemVariant::Trait)?;
        }
        Ok(())
    }

    pub fn push_use(&self, parser: &mut Parser) -> Result<(), ParseError> {
        // push uses
        for u in self.uses.iter() {
            parser.env.push_use(u.path.clone())?;
        }
        Ok(())
    }



    /// Parses all structs in the pre-module to proper `AstStruct`s and collects the result into
    /// a `Vec`.
    fn parse_types(&self, parser: &mut Parser) -> Result<Vec<AstTypeDef>, ParseError> {
        let mut structs = vec![];
        for s in self.types.iter() {
            parser.reset_until(&s.pos)?;
            structs.push(AstTypeDef::parse(parser)?);
        }
        Ok(structs)
    }

    fn parse_funcs(&self, parser: &mut Parser) -> Result<Vec<AstFunc>, ParseError> {
        let mut funcs = vec![];
        for f in self.funcs.iter() {
            parser.reset_until(&f.pos)?;
            funcs.push(AstFunc::parse(parser)?);
        }
        Ok(funcs)
    }

    fn parse_impl(&self, parser: &mut Parser) -> Result<Vec<AstImpl>, ParseError> {
        let mut impls = vec![];
        for im in self.impls.iter() {
            parser.reset_until(&im.pos)?;
            impls.push(AstImpl::parse(parser)?);
        }
        Ok(impls)
    }

    fn parse_traits(&self, parser: &mut Parser) -> Result<Vec<AstTrait>, ParseError> {
        let mut traits = vec![];
        for tra in self.traits.iter() {
            parser.reset_until(&tra.pos)?;
            traits.push(AstTrait::parse(parser)?);
        }
        Ok(traits)
    }
}





#[cfg(test)]
mod test {
    use crate::parser::{ParseError, Parser};
    use crate::parser::ast::Parsable;
    use crate::parser::pre_parse::PreModule;

    #[test]
    fn test_pre_parser() -> Result<(), ParseError> {
        let src = r#"
        type Job = enum {
            Physicist,
            SoftwareDeveloper,
        }

        type Person = struct {
            name: *const str,
            age: u32,
            job: Job,
        }

        trait Display {
            type Output;

            fn display(*const self) -> *const Self::Output;
        }

        impl Display for Person {
            type Output = str;

            fn display(*const self) -> str {
                self.name
            }
        }

        impl Person {
            fn new(name: *const str, age: u32, job: Job) -> Person {
                Person {
                    name, age, job
                }
            }
        }

        fn main() {
            std::println("Hello, World!");
            let data = Person { name: "Dieter", age: 32, job: Physicist };
            data.display();
        }
        "#;

        let mut parser = Parser::new(src);
        let module = PreModule::parse(&mut parser, "test".to_string())?;

        println!("pre-parse module: {:#?}", module);

        // parse structs
        parser.env.revert_to_scope(&module.stack_level);
        let types = module.parse_types(&mut parser)?;
        let traits = module.parse_traits(&mut parser)?;
        let funcs = module.parse_funcs(&mut parser)?;
        let impls = module.parse_impl(&mut parser)?;

        println!("{:#?}", types);
        println!("{:#?}", funcs);
        println!("{:#?}", impls);
        println!("{:#?}", traits);

        parser.env.pop();
        Ok(())
    }
}
