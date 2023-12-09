use std::{fs, io};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::path::Path;
use std::rc::Rc;
use crate::lexer::SrcPos;
use crate::parser::ast::ast_func::AstFunc;
use crate::parser::ast::ast_impl::AstImpl;
use crate::parser::ast::ast_trait::AstTrait;
use crate::parser::ast::ast_type_def::AstTypeDef;
use crate::parser::ast::ast_use::AstUse;
use crate::parser::{ParseError, Parser};
use crate::parser::pre_parse::{PreModule};
use crate::parser::resolver::TopLevelNameResolver;


pub struct ModuleParser {
    resolver: TopLevelNameResolver,
}

#[derive(Debug)]
pub struct AstModule {
    name: String,
    pos: Option<SrcPos>,

    types: Vec<AstTypeDef>,
    impls: Vec<AstImpl>,
    traits: Vec<AstTrait>,
    funcs: Vec<AstFunc>,
    uses: Vec<AstUse>,

    submodules: Vec<AstModule>,
}

pub trait CodeSource<'a>: Debug {
    fn src(&self) -> Result<String, io::Error>;
}


#[derive(Debug)]
pub struct SrcTreeElement<'a> {
    name: String,
    src: Box<dyn CodeSource<'a>>,
    subs: Vec<SrcTreeElement<'a>>,
}


#[derive(Debug, Clone)]
pub enum FileError {
    NotFound,
    InvalidName(String),
    IOError(Rc<io::Error>),
}

impl Display for FileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FileError::NotFound => {
                write!(f, "File not found")
            }
            FileError::InvalidName(name) => {
                write!(f, "File with name {name} not found")
            }
            FileError::IOError(e) => {
                write!(f, "IO error: {e}")
            }
        }
    }
}

impl Error for FileError {}

impl From<FileError> for ParseError {
    fn from(value: FileError) -> Self {
        ParseError::FileError(value)
    }
}

impl<'a> SrcTreeElement<'a> {
    pub fn new<Code: CodeSource<'a> + 'static>(name: String, src: Code) -> Self {
        SrcTreeElement {
            name,
            src: Box::new(src),
            subs: Vec::new(),
        }
    }

    pub fn from_file(file: String, suffix: &str) -> Result<Self, FileError> {
        let path: &Path = Path::new(&file);
        if path.is_file() {
            // read from file
            let name = path.file_name().unwrap().to_string_lossy().to_string();
            if !name.ends_with(suffix) {
                return Err(FileError::InvalidName(name));
            }
            let name = &name[0..(name.len() - suffix.len())];
            let mut element = Self::new(name.to_string(), FileSource::new(file.clone()));

            // try to find submodules
            let path: &Path = Path::new(&file);
            let path = path.with_file_name(name);
            if let Ok(dirs) = path.read_dir() {
                for child in dirs {
                    match child {
                        Ok(child) if child.path().is_file() => {
                            element.push(Self::from_file(
                                child.path().into_os_string().into_string().unwrap(),
                                suffix)?
                            );
                        },
                        _ => (),
                    }
                }
            }
            Ok(element)
        } else {
            Err(FileError::NotFound)
        }
    }

    pub fn push(&mut self, el: SrcTreeElement<'a>) {
        self.subs.push(el);
    }
}

#[derive(Debug)]
pub struct SourceTree<'a> {
    roots: Vec<SrcTreeElement<'a>>,
}

impl<'a> SourceTree<'a> {
    pub fn new() -> Self {
        SourceTree {
            roots: Vec::new(),
        }
    }

    pub fn push(&mut self, child: SrcTreeElement<'a>) {
        self.roots.push(child);
    }
}


#[derive(Debug, Clone)]
struct FileSource {
    path: String
}

impl FileSource {
    pub fn new(path: String) -> Self {
        FileSource {
            path,
        }
    }
}

impl<'a> CodeSource<'a> for FileSource {
    fn src(&self) -> Result<String, io::Error> {
        fs::read_to_string(&self.path)
    }
}

impl<'a> CodeSource<'a> for &'a str {
    fn src(&self) -> Result<String, io::Error> {
        Ok(self.to_string())
    }
}


#[derive(Debug)]
struct PreParseModule<'a, 'b> {
    src: &'a SrcTreeElement<'b>,
    code: String,
    module: PreModule,
    children: Vec<PreParseModule<'a, 'b>>,
}

impl ModuleParser {
    pub fn new() -> Self {
        let mut resolver = TopLevelNameResolver::new();
        ModuleParser {
            resolver,
        }
    }

    pub fn parser(&mut self, tree: SourceTree) -> Result<Vec<AstModule>, ParseError> {
        // pre-parse phase
        let mut pre_modules = Vec::new();
        for el in tree.roots.iter() {
            pre_modules.push(self.pre_parse_element(el)?);
        }
        // parse phase
        let mut modules = Vec::new();
        for pre in pre_modules {
            modules.push(self.parse_element(pre)?);
        }
        Ok(modules)
    }

    fn pre_parse_element<'a, 'b>(
        &mut self,
        el: &'a SrcTreeElement<'b>
    ) -> Result<PreParseModule<'a, 'b>, ParseError> {
        let src = el.src.src().map_err(|e| ParseError::IoError(Rc::new(e)))?;
        let mut parser = Parser::with_env(&src, &mut self.resolver);

        // pre-parse module and submodules
        println!("Parsing file: {}", el.name);
        let m = PreModule::parse(&mut parser, el.name.clone())?;
        self.resolver.revert_to_scope(&m.stack_level);

        // parse children
        let mut children = Vec::new();
        for child in el.subs.iter() {
            let child_module = self.pre_parse_element(child)?;
            children.push(child_module);
        }

        self.resolver.pop();
        Ok(PreParseModule {
            src: el,
            code: src,
            module: m,
            children,
        })
    }

    fn parse_element(&mut self, el: PreParseModule) -> Result<AstModule, ParseError> {
        let mut parser = Parser::with_env(&el.code, &mut self.resolver);
        parser.env.revert_to_scope(&el.module.stack_level);
        el.module.push_use(&mut parser)?;
        // parse items
        let types = el.module.parse_types(&mut parser)?;
        let traits = el.module.parse_traits(&mut parser)?;
        let funcs = el.module.parse_funcs(&mut parser)?;
        let impls = el.module.parse_impl(&mut parser)?;

        // parse children
        let mut children = Vec::new();
        for c in el.children {
            children.push(self.parse_element(c)?);
        }

        self.resolver.pop();
        Ok(AstModule {
            name: el.module.name.unwrap(),
            pos: el.module.pos,
            types,
            impls,
            traits,
            funcs,
            uses: el.module.uses,
            submodules: children,
        })
    }
}




#[cfg(test)]
mod test {
    use crate::parser::module_parser::{ModuleParser, SourceTree, SrcTreeElement};
    use crate::parser::ParseError;

    fn parse() -> Result<(), ParseError> {
        // load file tree
        let mut tree = SourceTree::new();
        tree.push(SrcTreeElement::from_file("lang/test.nc".to_string(), ".nc")?);
        tree.push(SrcTreeElement::from_file("lang/std.nc".to_string(), ".nc")?);
        println!("{tree:?}");

        // parse
        let mut parser = ModuleParser::new();
        let modules = parser.parser(tree)?;
        // check modules
        println!("{modules:?}");

        Ok(())
    }

    #[test]
    fn test_parsing() {
        match parse() {
            Ok(()) => (),
            Err(e) => {
                println!("{e}");
                panic!("{e}");
            }
        }
    }
}
