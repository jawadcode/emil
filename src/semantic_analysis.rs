use std::collections::HashMap;

use lasso::Spur;

pub struct Analyser {
    scopes: Vec<Scope>,
}

pub struct Scope {
    type_env: HashMap<Spur, Type>,
    env: HashMap<Spur, Type>,
}

pub enum Type {
    Int,
    Real,
    Bool,
    Char,
    Ident(Spur),
    Enum(Vec<Spur>),
    Array { index: Box<Type>, elem: Box<Type> },
    Record(HashMap<Spur, Type>),
    Set(Box<Type>),
    File(Box<Type>),
}

impl Analyser {
    fn test() {}
}
