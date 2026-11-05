use std::fmt::Debug;

use crate::{ast::UnspanIdent, sema::context::TypeId};

pub mod expr;
pub mod program;
pub mod stmt;

#[derive(Debug, Clone)]
pub struct Typed<T: Debug + Clone> {
    pub node: T,
    pub r#type: TypeId,
}

impl<T: Debug + Clone> Typed<T> {
    pub fn map<U: Debug + Clone>(self, f: impl FnOnce(T) -> U) -> Typed<U> {
        Typed { node: f(self.node), r#type: self.r#type }
    }
}

pub type TypedIdent = Typed<UnspanIdent>;
