mod namespace;

use std::any::TypeId;
use std::collections::{HashMap};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use rand::{Rng, thread_rng};
use crate::lexer::SrcPos;
use crate::parser::ast::ast_type::{AstType, QualifierName};
use crate::parser::func::FuncSignature;
use crate::parser::r#type::{LangTypeId, Type, TypeRegistry, TypeSignature};
use crate::parser::resolver::namespace::{Namespace};


#[derive(Clone, Debug)]
pub enum ResolveError {
    ItemRedefinition(QualifierName, LangTypeId),
    TypeRegistry(Rc<dyn TypeSignature>, SrcPos),
    EmptyTypeRegistry(SrcPos),
    EmptyStack,
    InvalidUse(QualifierName),
    UnknownItem(QualifierName),
    EmptyQualifierName,
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
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
            ResolveError::ItemRedefinition(name, ty) => {
                write!(f, "Attempted to redefine type with id {ty:?} and name {name}")
            }
            ResolveError::UnknownItem(name) => {
                write!(f, "Unknown item with name {name}")
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
    self_type: Option<AstType>,
    items: Namespace<Item>,
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

    /// Resolves a local type based on the current scope
    fn resolve_local<'a>(
        &self,
        path: &QualifierName,
        namespace: &'a Namespace<Item>
    ) -> Option<&'a Item> {
        let mut qualifier_name = self.base_namespace.clone();
        for next in path.iter() {
            qualifier_name.push(next.clone());
        }
        namespace.resolve(&qualifier_name)
    }

    /// Resolves a qualifier path to an item using the specified namespace hierarchy and local
    /// imports / `use` statements.
    fn resolve_type<'a>(
        &self,
        path: &QualifierName,
        namespace: &'a Namespace<Item>
    ) -> Option<&'a Item> {
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
    fn insert_namespace(
        &mut self,
        namespace: &Namespace<Item>,
        path: QualifierName
    ) -> Result<(), ResolveError> {
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
    pub type_id: LangTypeId,
    pub scope: ScopeId,
    pub variant: ItemVariant,
}


#[derive(Debug, Clone, PartialEq, PartialOrd)]
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

    /// Changes the current scope of the resolver to the specified path.
    /// If the path does not exist in the resolver, an error is returned.
    pub fn scope_to_path(&mut self, path: &QualifierName) -> Result<(), ResolveError> {
        let item = self.find_item(path)
            .ok_or(ResolveError::InvalidUse(path.clone()))?;
        let scope = item.scope.clone();
        self.revert_to_scope(&scope);
        Ok(())
    }

    pub fn push_module(&mut self, name: String) {
        self.push(Some(name), true, None);
    }

    pub fn push_type(&mut self, name: String) {
        self.push(Some(name.clone()), false, Some(AstType::Base(name.into())));
    }

    pub fn push_fn(&mut self, name: String) {
        self.push(Some(name), false, None);
    }

    pub fn push_impl(&mut self, ty_name: AstType) {
        self.push(None, false, Some(ty_name));
    }

    /// Pushes a new stack level to the resolver.
    /// Each stack level contains its own namepsace.
    /// Thus, a stack level in this context is a namespace in the code.
    /// This can be any named item, except for functions:
    ///
    /// - modules,
    /// - traits
    /// - types (structs, enums, type alias)
    fn push(
        &mut self,
        namespace_name: Option<String>,
        is_module: bool,
        self_type: Option<AstType>
    ) {
        // find new id
        let mut id = ScopeId::rand();
        while self.stack.memory.contains_key(&id) {
            id = ScopeId::rand();
        }
        // create new namespace name by appending the name of the namespace to the name of the
        // last namespace
        let mut name = if !self.stack.stack.is_empty() {
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
        self.stack.memory.insert(
            id,
            StackLvl::new(id, name, self.stack.stack.clone(), is_module, self_type)
        );
    }

    /// Searches the levels of the resolver stack top to bottom and tries to resolve the item.
    pub fn find_item(&self, name: &QualifierName) -> Option<&Item> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;

        stack.rev_iter(move |lvl| {
            lvl.find_item(name, namespace)
        }, || None)
    }

    pub fn find_local_item(&self, name: &QualifierName) -> Option<&Item> {
        let TopLevelNameResolver {
            stack,
            namespace,
            ..
        } = self;

        let lvl = stack.current_level().ok()?;
        lvl.find_local_item(name, &namespace)
    }

    pub fn find_top_level_type(&self, name: &QualifierName) -> Option<LangTypeId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { type_id, variant: ItemVariant::Type, .. }) = self.find_item(name) {
            Some(*type_id)
        } else {
            None
        }
    }

    pub fn find_trait(&self, name: &QualifierName) -> Option<LangTypeId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { type_id, variant: ItemVariant::Trait, .. }) = self.find_item(name) {
            Some(*type_id)
        } else {
            None
        }
    }

    pub fn find_top_level_function(&self, name: &QualifierName) -> Option<LangTypeId> {
        // go through levels from top to bottom and try to resolve the reference to the item
        if let Some(Item { type_id, variant: ItemVariant::Function, .. }) = self.find_item(name) {
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
    ) -> Result<ScopeId, ResolveError> {
        // check if the item is already available in the scope
        if let Some(item) = self.find_item(&name.clone().into()) {
            return Err(ResolveError::ItemRedefinition(name.into(), item.type_id));
        }

        // register type
        let id = TypeRegistry::global().add_empty()
            .ok_or(ResolveError::EmptyTypeRegistry(src))?;

        // form fully qualified name
        let level = self.stack.current_level()?;
        let mut full_name = level.imports.base_namespace.clone();
        full_name.push(name.clone());

        // debug
        println!("Pushing item {} to resolver", full_name);

        // push now stack entry for that item onto the stack
        match &variant {
            ItemVariant::Type | ItemVariant::Trait => self.push_type(name),
            ItemVariant::Function => self.push_fn(name),
            ItemVariant::Const => self.push_fn(name), // treat constants as comp-time functions for now
        }
        let scope_id = self.stack.current_scope()?.clone();
        self.pop();

        // insert item into the namespace hierarchy
        self.namespace.insert_item(&full_name, Item { type_id: id, variant, scope: scope_id });
        Ok(scope_id)
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
        self_type: Option<AstType>,
    ) -> Self {
        StackLvl {
            id,
            imports: Imports::new(base_module),
            stack,
            is_module,
            self_type,
            items: Namespace::new("".to_string()),
        }
    }

    fn find_self_type(&self) -> Option<QualifierName> {
        let ty = self.self_type.as_ref()?;
        match ty {
            AstType::Base(base) => {
                let name: QualifierName = base.into();
                Some(name)
            }
            AstType::Ptr(_) => None,
            AstType::MutPtr(_) => None,
            AstType::Array(_, _) => None,
            AstType::Slice(_) => None,
        }
    }

    fn find_local_item<'a>(&self, name: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        self.imports.resolve_local(name, namespace)
    }

    fn find_item<'a>(&self, name: &QualifierName, namespace: &'a Namespace<Item>) -> Option<&'a Item> {
        let path: QualifierName = name.clone();
        let mut iter = path.iter();
        match iter.next() {
            Some(first) if first == "Self" => {
                let self_ty = self.find_self_type()?;

                // check if there is more
                if let Some(latter) = iter.next() {
                    // collect latter & check
                    let mut path = self_ty;
                    path.push(latter.clone());
                    for i in iter {
                        path.push(i.clone());
                    }

                    // now look in the namespace tree
                    namespace.resolve(&path)
                } else {
                    // return the base type of the stack lvl
                    self.imports.resolve_type(&self_ty, namespace)
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
    use crate::parser::resolver::TopLevelNameResolver;

    #[test]
    fn test() -> Result<(), ParseError> {
        let mut env = TopLevelNameResolver::new();
        let mut parser = Parser::with_env(r#"
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
        "#, &mut env);

        let expr = AstExpression::parse(&mut parser);
        match expr {
            Ok(expr) => println!("{expr:#}"),
            Err(e) => println!("ERR: {e:#}"),
        }
        Ok(())
    }
}
