use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::parser::r#type::{Type, TypeError, TypeSignature, TypeSize};

#[derive(Clone, Debug, Eq, PartialEq)]
struct StructMember {
    name: String,
    ty: Type,
    offset: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructSignature {
    members: HashMap<String, StructMember>,
    name: String,
    alignment: usize,
    size: usize,
}


#[derive(Clone, Debug, Eq, PartialEq)]
pub struct UnionSignature {
    name: String,
    variants: HashMap<String, Type>,
    alignment: usize,
    size: usize,
}


impl StructSignature {
    pub fn new(name: String) -> Self {
        StructSignature {
            members: HashMap::new(),
            name,
            alignment: 8,
            size: 0,
        }
    }

    pub fn add_member(&mut self, name: String, ty: Type) -> Result<(), TypeError> {
        self.alignment = usize::max(self.alignment, ty.alignment());
        let aligned_size = Self::align(ty.size_or()?, self.alignment);
        // adjust struct size and alignment
        self.size = Self::align(self.size, self.alignment);
        // insert member
        let mem = StructMember {
            ty,
            name: name.clone(),
            offset: self.size,
        };
        self.members.insert(name, mem);
        self.size += aligned_size;
        Ok(())
    }

    pub fn align(element_size: usize, alignment: usize) -> usize {
        alignment * ((element_size + alignment - 1) / alignment)
    }
}

impl UnionSignature {
    pub fn new(name: String) -> Self {
        UnionSignature {
            name,
            variants: HashMap::new(),
            alignment: 8,
            size: 0,
        }
    }

    pub fn add_variant(&mut self, name: String, ty: Type) -> Result<(), TypeError> {
        self.alignment = usize::max(self.alignment, ty.alignment());
        let aligned_size = StructSignature::align(ty.size_or()?, self.alignment);
        // adjust union size and alignment
        self.size = usize::max(
            StructSignature::align(self.size, self.alignment),
            aligned_size
        );
        // insert variant
        self.variants.insert(name, ty);
        Ok(())
    }
}

impl Display for UnionSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "union {}", self.name)
    }
}

impl TypeSignature for UnionSignature {
    fn size(&self) -> TypeSize {
        TypeSize::Sized(self.size)
    }

    fn alignment(&self) -> usize {
        self.alignment
    }
}

impl Display for StructSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "struct {}", self.name)
    }
}

impl TypeSignature for StructSignature {
    fn size(&self) -> TypeSize {
        TypeSize::Sized(self.size)
    }

    fn alignment(&self) -> usize {
        self.alignment
    }
}
