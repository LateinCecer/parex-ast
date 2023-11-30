use std::any::TypeId;
use std::collections::{HashMap};
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use rand::{Rng, thread_rng};
use crate::lexer::SrcPos;
use crate::parser::ast::ast_type::TypeName;
use crate::parser::func::FuncSignature;
use crate::parser::r#type::{LangTypeId, Type, TypeRegistry, TypeSignature};


#[derive(Clone, Debug)]
pub enum ResolveError {
    StructRedefinition(Box<StructDef>, SrcPos),
    FunctionRedefinition(Box<FnDef>, SrcPos),
    TypeRegistry(Rc<dyn TypeSignature>, SrcPos),
    EmptyTypeRegistry(SrcPos),
    EmptyStack,
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
    struct_names: HashMap<String, LangTypeId>,
    structs: HashMap<LangTypeId, StructDef>,

    variable_names: HashMap<String, LangTypeId>,
    variables: HashMap<LangTypeId, VarDef>,

    fn_names:  HashMap<String, LangTypeId>,
    fns: HashMap<LangTypeId, FnDef>,

    methods: HashMap<Type, ImplCollection>,
    id: ScopeId,
}

pub struct NameResolver {
    stack: Vec<ScopeId>,
    memory: HashMap<ScopeId, StackLvl>,
}

impl NameResolver {
    pub fn new() -> Self {
        NameResolver {
            stack: Vec::new(),
            memory: HashMap::new(),
        }
    }

    pub fn current_scope(&self) -> Result<&ScopeId, ResolveError> {
        self.stack.last().ok_or(ResolveError::EmptyStack)
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn push(&mut self) {
        // find new id
        let mut id = ScopeId::rand();
        while self.memory.contains_key(&id) {
            id = ScopeId::rand();
        }
        // push new resolve level
        self.memory.insert(id, StackLvl::new(id));
        self.stack.push(id);
    }

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
        }
        a()
    }

    pub fn find_var(&self, name: &String) -> Option<LangTypeId> {
        self.rev_iter(move |lvl| lvl.find_var(name).cloned(), || None)
    }

    pub fn find_struct(&self, name: &TypeName) -> Option<LangTypeId> {
        // check if the **last** parameter is a struct
        if name.len() == 1 {
            if let Some(val) = self.rev_iter(move |lvl| {
                let tmp = lvl.find_struct(name).cloned()?;
                let tmp = lvl.get_struct(&tmp)?;
                Some(tmp.id)
            }, || None) {
                return Some(val);
            }
        }

        // check if the **second to last** parameter is an enum and if the **last** parameter is
        // a variant of this enum
        // if name.len() == 2 {
        //     if let Some(val) = self.rev_iter(move |lvl| {
        //         let tmp = lvl.find_struct(name.first().unwrap()).cloned()?;
        //         let tmp = lvl.get_struct(&tmp)?;
        //         if tmp.ty == StructType::Union() {
        //             Some(tmp.id)
        //         } else {
        //             None
        //         }
        //     }, || None) {
        //         return Some(val);
        //     }
        // }

        // recursively resolve other paths in scope (todo)
        None
    }

    pub fn find_fn(&self, name: &TypeName) -> Option<LangTypeId> {
        // check if there is more than one name
        if name.len() == 1 {
            self.rev_iter(move |lvl| lvl
                .find_fn(name).cloned(), || None)
        } else {
            unimplemented!()
        }
    }

    pub fn push_var(&mut self, name: String, src: SrcPos) -> Result<(), ResolveError> {
        if let Some(lvl) = self.stack.last()
            .map(|idx| self.memory.get_mut(idx)).flatten() {

            lvl.push_var(name, src);
            Ok(())
        } else {
            Err(ResolveError::EmptyStack)
        }
    }

    pub fn push_fn(&mut self, sig: FuncSignature, src: SrcPos) -> Result<(), ResolveError> {
        if let Some(lvl) = self.stack.last()
            .map(|idx| self.memory.get_mut(idx)).flatten() {

            lvl.push_fn(sig, src)
        } else {
            Err(ResolveError::EmptyStack)
        }
    }

    pub fn push_struct(&mut self, name: TypeName, src: SrcPos) -> Result<(), ResolveError> {
        if let Some(lvl) = self.stack.last()
            .map(|idx| self.memory.get_mut(idx)).flatten() {

            lvl.push_struct(name, src)
        } else {
            Err(ResolveError::EmptyStack)
        }
    }

    pub fn push_alias(&mut self, name: TypeName, src: SrcPos) -> Result<(), ResolveError> {
        todo!()
    }
}

impl StackLvl {
    pub fn new(id: ScopeId) -> Self {
        StackLvl {
            struct_names: HashMap::new(),
            structs: HashMap::new(),

            variable_names: HashMap::new(),
            variables: HashMap::new(),

            fn_names: HashMap::new(),
            fns: HashMap::new(),

            methods: HashMap::new(),
            id,
        }
    }

    pub fn find_var(&self, name: &String) -> Option<&LangTypeId> {
        self.variable_names.get(name)
    }

    pub fn get_var(&self, id: &LangTypeId) -> Option<&VarDef> {
        self.variables.get(id)
    }

    pub fn find_struct(&self, name: &TypeName) -> Option<&LangTypeId> {
        self.struct_names.get(&name.first().unwrap().name)
    }

    pub fn get_struct(&self, id: &LangTypeId) -> Option<&StructDef> {
        self.structs.get(id)
    }

    pub fn find_fn(&self, name: &TypeName) -> Option<&LangTypeId> {
        self.fn_names.get(&name.first().unwrap().name)
    }

    pub fn get_fn(&self, id: &LangTypeId) -> Option<&FnDef> {
        self.fns.get(id)
    }

    pub fn push_var(&mut self, name: String, src: SrcPos) {
        // find new variable id
        let mut id = LangTypeId::rand();
        while self.variables.contains_key(&id) {
            id = LangTypeId::rand();
        }
        self.variables.insert(id, VarDef {
            id,
            name: name.clone(),
            pos: src,
        });

        // insert name definition
        let entry = self.variable_names.entry(name);
        *entry.or_insert(id) = id;
    }

    pub fn push_fn(&mut self, sig: FuncSignature, src: SrcPos) -> Result<(), ResolveError> {
        // register type
        let id = TypeRegistry::global().add_signature(sig.clone())
            .ok_or(ResolveError::TypeRegistry(
                Rc::new(sig.clone()), src))?;

        let def = FnDef {
            id,
            sig: sig.clone(),
            pos: src,
        };

        if let Some(name) = &sig.name {
            // see if function is already registered
            if self.fn_names.get(name).is_some() {
                return Err(ResolveError::FunctionRedefinition(Box::new(def), src))
            }

            // insert fn definition
            self.fns.insert(id, def);

            // insert name definition
            let entry = self.fn_names.entry(name.clone());
            *entry.or_insert(id) = id;
        }
        Ok(())
    }

    pub fn push_struct(&mut self, name: TypeName, src: SrcPos) -> Result<(), ResolveError> {
        // register type
        let id = TypeRegistry::global().add_empty()
            .ok_or(ResolveError::EmptyTypeRegistry(src))?;

        // todo: fix nested name spaces
        let simple_name = name.last().unwrap().clone().name;
        let def = StructDef {
            id,
            name: simple_name.clone(),
            pos: src
        };
        if self.struct_names.contains_key(&simple_name) {
            return Err(ResolveError::StructRedefinition(Box::new(def), src));
        }

        // insert struct definition
        self.structs.insert(id, def);

        // insert name definition
        let entry = self.struct_names.entry(simple_name);
        *entry.or_insert(id) = id;
        Ok(())
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
