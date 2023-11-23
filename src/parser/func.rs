use std::fmt::{Debug, Display, Formatter};
use crate::parser::r#type::{Type, TypeSignature, TypeSize};


#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub struct FuncSignature {
    pub name: Option<String>,
    pub params: Vec<Type>,
    pub ret: Type,
}

impl Display for FuncSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // format parameters
        let mut param_format = "".to_owned();
        if !self.params.is_empty() {
            let mut itr = self.params.iter();
            param_format.push_str(&format!("{}", itr.next().unwrap()));

            loop {
                match itr.next() {
                    Some(ty) => param_format.push_str(&format!(", {ty}")),
                    _ => break,
                }
            }
        }

        if let Some(name) = &self.name {
            write!(f, "{name}({param_format}) -> {}", self.ret)
        } else {
            write!(f, "|{param_format}| -> {}", self.ret)
        }
    }
}


impl TypeSignature for FuncSignature {
    fn size(&self) -> TypeSize {
        todo!()
    }

    fn alignment(&self) -> usize {
        todo!()
    }
}

