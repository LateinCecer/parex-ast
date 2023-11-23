pub mod core;

use std::any::{TypeId};
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::sync::{Arc, Mutex, OnceLock};
use rand::Rng;
use crate::parser::parameter_env::ParameterId;
use crate::parser::resolver::ScopeId;




#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash, Debug)]
pub struct LangTypeId(usize, usize);

impl LangTypeId {
    pub fn rand() -> Self {
        let mut rng = rand::thread_rng();
        LangTypeId(rng.gen(), rng.gen())
    }
}



#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum TypeVariant {
    Base(LangTypeId),
    Ptr(Box<Type>),
    MutPtr(Box<Type>),
    Array(Box<Type>, usize),
    Slice(Box<Type>),
    Generic(ParameterId),
}


#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Lifetime {
    Scope(ScopeId),
}

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct Type {
    pub var: TypeVariant,
    pub lifetime: Lifetime,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    NoSuchType(LangTypeId),
    MustBeSized(Type),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoSuchType(id) => write!(f, "Type with id '{:?}' does not exist", id),
            Self::MustBeSized(ty) => write!(f, "Type '{ty}' is required to be sized, but is not"),
        }
    }
}


impl Type {
    pub fn base(id: LangTypeId, lt: Lifetime) -> Self {
        Type {
            var: TypeVariant::Base(id),
            lifetime: lt,
        }
    }

    pub fn ptr(ty: Type, lt: Lifetime) -> Self {
        Type {
            var: TypeVariant::Ptr(Box::new(ty)),
            lifetime: lt,
        }
    }

    pub fn mut_ptr(ty: Type, lt: Lifetime) -> Self {
        Type {
            var: TypeVariant::MutPtr(Box::new(ty)),
            lifetime: lt,
        }
    }

    pub fn array(ty: Type, len: usize, lt: Lifetime) -> Self {
        Type {
            var: TypeVariant::Array(Box::new(ty), len),
            lifetime: lt,
        }
    }

    pub fn slice(ty: Type, lt: Lifetime) -> Self {
        Type {
            var: TypeVariant::Slice(Box::new(ty)),
            lifetime: lt,
        }
    }
}

impl Display for Lifetime {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "") // todo implement properly
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type { var: TypeVariant::Base(id), lifetime } => {
                let ty = TypeRegistry::global().get_raw(id).unwrap();
                write!(f, "('{lifetime} {ty})")
            }
            Type { var: TypeVariant::Ptr(base), lifetime } => {
                write!(f, "(*'{lifetime} {base})")
            }
            Type { var: TypeVariant::MutPtr(base), lifetime } => {
                write!(f, "(*mut '{lifetime} {base})")
            }
            Type { var: TypeVariant::Array(base, len), lifetime: _ } => {
                write!(f, "[{base}; {len}]")
            }
            Type { var: TypeVariant::Slice(base), lifetime: _ } => {
                write!(f, "[{base}]")
            }
            Type { var: TypeVariant::Generic(id), lifetime: _ } => {
                write!(f, "<{id}>")
            }
        }
    }
}

impl Type {
    pub fn size(&self) -> TypeSize {
        todo!()
    }

    pub fn size_or(&self) -> Result<usize, TypeError> {
        match self.size() {
            TypeSize::Sized(s) => Ok(s),
            TypeSize::Unsized => Err(TypeError::MustBeSized(self.clone()))
        }
    }

    pub fn alignment(&self) -> usize {
        todo!()
    }
}


pub enum TypeSize {
    Sized(usize),
    Unsized
}



///
pub trait TypeSignature: Display + Sync + Send + Debug {
    fn size(&self) -> TypeSize;
    fn alignment(&self) -> usize;
}

static TYPE_REG: OnceLock<TypeRegistry> = OnceLock::new();


/// A signature reflection can be used to convert Rust types to internal language types.
#[derive(Clone)]
pub struct SignatureReflection {
    id: TypeId,
    core: Arc<dyn TypeSignature>,
}



impl SignatureReflection {
    pub fn get<T: TypeSignature + 'static>(&self) -> Option<&T> {
        if self.id == TypeId::of::<T>() {
            let r = self.core.as_ref() as *const dyn TypeSignature;
            Some(unsafe {
                &*(r as *const T)
            })
        } else {
            None
        }
    }
}



/// The type registry contains information about the types that are accessible to the system.
/// Essentially, it associates `LangTypeId`s with the type signatures.
pub struct TypeRegistry {
    types: Mutex<HashMap<LangTypeId, Option<SignatureReflection>>>,
}

impl TypeRegistry {
    pub fn global() -> &'static Self {
        TYPE_REG.get_or_init(|| TypeRegistry { types: Mutex::new(HashMap::new()) })
    }

    pub fn add_signature<T: TypeSignature + 'static>(&self, sig: T) -> Option<LangTypeId> {
        if let Ok(mut map) = self.types.lock() {
            // find id
            let mut id = LangTypeId::rand();
            while map.contains_key(&id) {
                id = LangTypeId::rand();
            }
            // insert signature and return id
            map.insert(id, Some(SignatureReflection {
                id: TypeId::of::<T>(),
                core: Arc::new(sig)
            }));
            Some(id)
        } else {
            None
        }
    }

    pub fn add_empty(&self) -> Option<LangTypeId> {
        if let Ok(mut map) = self.types.lock() {
            // find id
            let mut id = LangTypeId::rand();
            while map.contains_key(&id) {
                id = LangTypeId::rand();
            }
            // insert signature and return id
            map.insert(id, None);
            Some(id)
        } else {
            None
        }
    }

    pub fn populate_signature<T: TypeSignature + 'static>(&self, ty: &LangTypeId, sig: T) -> Option<()> {
        if let Ok(mut map) = self.types.lock() {
            // find id
            *map.get_mut(ty)? = Some(SignatureReflection {
                id: TypeId::of::<T>(),
                core: Arc::new(sig)
            });
            Some(())
        } else {
            None
        }
    }

    pub fn get_raw(&self, id: &LangTypeId) -> Option<Arc<dyn TypeSignature>> {
        self.types.lock().map(|map| map.get(id).cloned())
            .ok().flatten().flatten()
            .map(|base| base.core.clone())
    }

    pub fn get(&self, id: &LangTypeId) -> Option<SignatureReflection> {
        self.types.lock().map(|map| map.get(id).cloned())
            .ok().flatten().flatten()
    }
}


