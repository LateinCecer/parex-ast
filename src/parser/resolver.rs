mod namespace;

use std::any::TypeId;
use std::collections::{HashMap};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use rand::{Rng, thread_rng};
use crate::lexer::SrcPos;
use crate::parser::ast::ast_type::{QualifierName, TypeName};
use crate::parser::func::FuncSignature;
use crate::parser::r#type::{LangTypeId, Type, TypeRegistry, TypeSignature};
use crate::parser::resolver::namespace::{Namespace};


#[derive(Clone, Debug)]
pub enum ResolveError {
    StructRedefinition(Box<StructDef>, SrcPos),
    FunctionRedefinition(Box<FnDef>, SrcPos),
    TypeRegistry(Rc<dyn TypeSignature>, SrcPos),
    EmptyTypeRegistry(SrcPos),
    EmptyStack,
    InvalidUse(QualifierName),
    EmptyQualifierName,
}

impl PartialEq for ResolveError {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ResolveError::StructRedefinition(def, pos) => {
                if let ResolveError::StructRedefinition(def_other, pos_other) = other {
                    def == def_other && pos == pos_other
                } else {
                    false
                }
            }
            ResolveError::FunctionRedefinition(def, pos) => {
                if let ResolveError::FunctionRedefinition(def_other, pos_other) = other {
                    def == def_other && pos == pos_other
                } else {
                    false
                }
            }
            ResolveError::TypeRegistry(_, pos) => {
                if let ResolveError::TypeRegistry(_, pos_other) = other {
                    pos == pos_other
                } else {
                    false
                }
            }
            ResolveError::EmptyTypeRegistry(pos) => {
                if let ResolveError::EmptyTypeRegistry(pos_other) = other {
                    pos == pos_other
                } else {
                    false
                }
            }
            ResolveError::EmptyStack => {
                if let ResolveError::EmptyStack = other {
                    true
                } else {
                    false
                }
            }
            ResolveError::InvalidUse(name) => {
                if let ResolveError::InvalidUse(other_name) = other {
                    name.clone() == other_name.clone()
                } else {
                    false
                }
            }
            ResolveError::EmptyQualifierName => {
                if let ResolveError::EmptyQualifierName = other {
                    true
                } else {
                    false
                }
            }
        }
    }
}

impl Eq for ResolveError {}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::StructRedefinition(def, src) => {
                write!(f, "Struct '{}' first defined at {} is redefined at {}", def.name, def.pos, src)
            }
            ResolveError::FunctionRedefinition(def, src) => {
                write!(f, "Function '{}' first defined at {} is redefined at {}", def.sig, def.pos, src)
            }
            ResolveError::TypeRegistry(sig, src) => {
                write!(f, "Unable to register type signature '{sig}' defined at {src}")
            }
            ResolveError::EmptyTypeRegistry(pos) => {
                write!(f, "Unable to register empty type signature defined at {pos}")
            }
            ResolveError::EmptyStack => {
                write!(f, "The resolver stack is empty")
            }
            ResolveError::InvalidUse(name) => {
                write!(f, "Qualifier name '{name}' does not point to a namespace or item in this project")
            }
            ResolveError::EmptyQualifierName => {
                write!(f, "Empty qualifier name")
            }
        }
    }
}



#[derive(Clone, Debug, Eq, PartialEq)]
pub enum StructType {
    Struct(),
    Union(),
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct StructDef {
    pos: SrcPos,
    name: String,
    id: LangTypeId,
}


#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
struct VarDef {
    pos: SrcPos,
    name: String,
    id: LangTypeId,
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
struct FnDef {
    pos: SrcPos,
    id: LangTypeId,
    sig: FuncSignature,
}

struct MethodDef {
    pos: SrcPos,
    id: LangTypeId,
    sig: FuncSignature,
    ty: Type,
}

struct Trait {
    fns: HashMap<String, FuncSignature>,
}

struct Implementation {
    fns: HashMap<String, MethodDef>,
}

struct TraitImplementation {
    tr: TypeId,
    fns: HashMap<String, MethodDef>,
}

struct ImplCollection {
    impls: Vec<Implementation>,
    trait_impls: Vec<TraitImplementation>,
}



#[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct ScopeId(usize, usize);

impl ScopeId {
    pub fn rand() -> Self {
        let mut rng = thread_rng();
        ScopeId(rng.gen(), rng.gen())
    }
}


struct StackLvl {
    id: ScopeId,
    imports: Imports,
    stack: Vec<ScopeId>,
    is_module: bool,
    items: Namespace<Item>,
    self_type: Option<QualifierName>,
}

struct Imports {
    namespaces: HashMap<String, QualifierName>,
    base_namespace: QualifierName,
}

impl Imports {
    fn new(base_namespace: QualifierName) -> Self {
        Imports {
            namespaces: HashMap::new(),
            base_namespace,
        }
    }

    fn self_type<'a>(&self, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        namespace.resolve(&self.base_namespace)
    }

    /// Resolves a qualifier path to an item using the specified namespace hierarchy and local
    /// imports / `use` statements.
    fn resolve_type<'a>(&self, path: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        if let Some(first) = path.first() {
            // look for first entry in imported namespaces and filter from there
            if let Some(search_path) = self.namespaces.get(first) {
                let mut iter = path.iter();
                iter.next(); // pop first element
                let mut path = search_path.clone();
                for next in iter {
                    path.push(next.clone());
                }
                // try to find item in the namespace tree
                if let Some(item) = namespace.resolve(&path) {
                    return Some(item);
                }
            }

            // try to resolve by prefixing the path with the local base namespace
            let mut qualifier_name = self.base_namespace.clone();
            for next in path.iter() {
                qualifier_name.push(next.clone());
            }
            if let Some(item) = namespace.resolve(&qualifier_name) {
                return Some(item);
            }

            // search from the bottom of the namespace instead
        }
        // see if there is anything in the root of the namespace hierarchy
        namespace.resolve(path)
    }


    /// Inserts a reference to a namespace.
    ///
    /// # Example
    ///
    /// In the code, this would look like this:
    /// ``
    /// use path::to::lib;
    /// ``
    fn insert_namespace(&mut self, namespace: &Namespace<Item>, path: QualifierName) -> Result<(), ResolveError> {
        if let Some(last) = path.last() {
            let last = last.to_owned();
            // check if the path leads to a namespace
            if namespace.is_namespace(&path) || namespace.is_item(&path) {
                self.namespaces.insert(last, path);
                Ok(())
            } else {
                Err(ResolveError::InvalidUse(path))
            }
        } else {
            Err(ResolveError::EmptyQualifierName)
        }
    }
}


pub struct Item {
    type_id: LangTypeId,
    variant: ItemVariant,
}

pub enum ItemVariant {
    Type,
    Function,
    Trait,
    Const,
}

struct Stack {
    stack: Vec<ScopeId>,
    memory: HashMap<ScopeId, StackLvl>,
}

impl Stack {
    fn new() -> Self {
        Stack {
            stack: Vec::new(),
            memory: HashMap::new(),
        }
    }

    fn revert_to_scope(&mut self, scope: &ScopeId) {
        if let Some(lvl) = self.memory.get(scope) {
            self.stack.clear();
            for &el in &lvl.stack {
                self.stack.push(el);
            }
        }
    }

    fn current_scope(&self) -> Result<&ScopeId, ResolveError> {
        self.stack.last().ok_or(ResolveError::EmptyStack)
    }

    fn current_level(&self) -> Result<&StackLvl, ResolveError> {
        self.stack.last()
            .and_then(|idx| self.memory.get(idx))
            .ok_or(ResolveError::EmptyStack)
    }

    fn current_level_mut(&mut self) -> Result<&mut StackLvl, ResolveError> {
        self.stack.last()
            .and_then(|idx| self.memory.get_mut(idx))
            .ok_or(ResolveError::EmptyStack)
    }

    /// Iterates top to bottom through the resolver stack until a module boundary is crossed, or the
    /// bottom of the stack is reached.
    fn rev_iter<F, A, R>(&self, f: F, a: A) -> Option<R>
        where
            F: Fn(&StackLvl) -> Option<R>,
            A: Fn() -> Option<R>, {

        for lvl in self.stack.iter().rev()
            .filter_map(|idx| self.memory.get(idx)) {

            let val = f(lvl);
            if val.is_some() {
                return val;
            }

            // break if a module boundary is crossed, as the local namespace is only available
            // within the scope of a single module.
            if lvl.is_module {
                break;
            }
        }
        a()
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }
}



pub struct TopLevelNameResolver {
    stack: Stack,
    namespace: Namespace<Item>,
}

impl TopLevelNameResolver {
    pub fn new() -> Self {
        let resolver = TopLevelNameResolver {
            stack: Stack::new(),
            namespace: Namespace::new("".to_string()),
        };
        resolver
    }

    pub fn current_scope(&self) -> Result<&ScopeId, ResolveError> {
        self.stack.current_scope()
    }

    pub fn revert_to_scope(&mut self, scope: &ScopeId) {
        self.stack.revert_to_scope(scope);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }


    /// Pushes a new stack level to the resolver.
    /// Each stack level contains its own namepsace.
    /// Thus, a stack level in this context is a namespace in the code.
    /// This can be any named item, except for functions:
    ///
    /// - modules,
    /// - traits
    /// - types (structs, enums, type alias)
    pub fn push(&mut self, namespace_name: Option<String>, is_module: bool) {
        // find new id
        let mut id = ScopeId::rand();
        while self.stack.memory.contains_key(&id) {
            id = ScopeId::rand();
        }
        // create new namespace name by appending the name of the namespace to the name of the
        // last namespace
        let mut name = if self.stack.stack.len() > 1 {
            if let Some(top) = self.stack.stack.last()
                .and_then(|id| self.stack.memory.get(id)) {
                top.imports.base_namespace.clone()
            } else {
                QualifierName::empty()
            }
        } else {
            QualifierName::empty()
        };
        // append namespace name to the base name
        if let Some(namespace_name) = namespace_name {
            name.push(namespace_name);
        }

        // push new resolve level
        self.stack.stack.push(id);
        self.stack.memory.insert(id, StackLvl::new(id, name, self.stack.stack.clone(), is_module));
    }

    /// Searches the levels of the resolver stack top to bottom and tries to resolve the item.
    pub fn find_item(&self, name: &TypeName) -> Option<&Item> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;

        stack.rev_iter(move |lvl| {
            lvl.find_item(name, namespace)
        }, || None)
    }

    pub fn find_top_level_type(&self, name: &TypeName) -> Option<LangTypeId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { type_id, variant: ItemVariant::Type }) = self.find_item(name) {
            Some(*type_id)
        } else {
            None
        }
    }

    pub fn find_trait(&self, name: &TypeName) -> Option<LangTypeId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { type_id, variant: ItemVariant::Trait }) = self.find_item(name) {
            Some(*type_id)
        } else {
            None
        }
    }

    pub fn find_top_level_function(&self, name: &TypeName) -> Option<LangTypeId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { type_id, variant: ItemVariant::Function }) = self.find_item(name) {
            Some(*type_id)
        } else {
            None
        }
    }

    /// Pushes top-level items to the namespace hierarchy of the namespace resolver.
    /// This includes:
    ///
    /// - Types,
    /// - Traits,
    /// - top-level Functions,
    /// - top-level Constants
    pub fn push_top_level_item(
        &mut self,
        name: String,
        src: SrcPos,
        variant: ItemVariant
    ) -> Result<(), ResolveError> {
        // register type
        let id = TypeRegistry::global().add_empty()
            .ok_or(ResolveError::EmptyTypeRegistry(src))?;

        // form fully qualified name
        let level = self.stack.current_level()?;
        let mut full_name = level.imports.base_namespace.clone();
        full_name.push(name);

        self.namespace.insert_item(&full_name, Item { type_id: id, variant });
        Ok(())
    }

    /// Adds a `use` statement to the current stack
    pub fn push_use(&mut self, name: QualifierName) -> Result<(), ResolveError> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;

        let lvl = stack.current_level_mut()?;
        lvl.imports.insert_namespace(&namespace, name)
    }
}

impl StackLvl {
    pub fn new(
        id: ScopeId,
        base_module: QualifierName,
        stack: Vec<ScopeId>,
        is_module: bool,
        self_type: Option<QualifierName>
    ) -> Self {
        StackLvl {
            id,
            imports: Imports::new(base_module),
            stack,
            is_module,
            items: Namespace::new("".to_string()),
            self_type,
        }
    }

    pub fn find_item<'a>(&self, name: &TypeName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        let path: QualifierName = name.clone().into();
        let mut iter = path.iter();
        match iter.next() {
            Some(first) if first == "Self" => {
                // check if there is more
                if let Some(latter) = iter.next() {
                    // collect latter & check


                    todo!()
                } else {
                    // return the base type of the stack lvl
                    self.imports.self_type(namespace)
                }
            },
            _ => self.imports.resolve_type(&path, namespace)
        }
    }
}



#[cfg(test)]
mod test {
    use crate::parser::ast::expression::AstExpression;
    use crate::parser::ast::Parsable;
    use crate::parser::{ParseError, Parser};

    #[test]
    fn test() -> Result<(), ParseError> {
        let mut parser = Parser::new(r#"
        {
            let mut x = 5;
            x = if x == 7 {
                std::print("Hello World!");
                9
            } else {
                1
            };

            match x {
                0 => std::print("0"),
                _ => {
                    std::print("Hello, world!")
                }
                1 => std::print("1"),
            }

            loop {
                if x == 0 {
                    break 1_usize;
                }
                x = x - 1;
                std::print("%g", x);
            }

            let _ = (x - 4).foo();

            1 - 2 as f32
        }
        "#);

        let expr = AstExpression::parse(&mut parser);
        match expr {
            Ok(expr) => println!("{expr:#}"),
            Err(e) => println!("ERR: {e:#}"),
        }
        Ok(())
    }
}
