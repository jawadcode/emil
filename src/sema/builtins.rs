use lasso::Rodeo;
use strum::{EnumIter, IntoStaticStr};

use crate::ast::UnspanIdent;

use super::context::{Constant, DefPoint};

pub trait Builtin: Into<&'static str> + Clone + Copy {
    const DEF_POINT_CONS: fn(Self) -> DefPoint;

    fn entry(self, rodeo: &mut Rodeo) -> (UnspanIdent, DefPoint) {
        let ident: &str = self.into();
        (rodeo.get_or_intern(ident), Self::DEF_POINT_CONS(self))
    }
}

#[derive(Debug, Clone, Copy, EnumIter, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum BuiltinConst {
    True,   // : boolean
    False,  // : boolean
    Maxint, // : integer
}

impl BuiltinConst {
    pub const fn get_type(self) -> BuiltinType {
        match self {
            BuiltinConst::True => BuiltinType::Boolean,
            BuiltinConst::False => BuiltinType::Boolean,
            BuiltinConst::Maxint => BuiltinType::Integer,
        }
    }
}

impl Builtin for BuiltinConst {
    const DEF_POINT_CONS: fn(Self) -> DefPoint = DefPoint::BuiltinConst;
}

impl From<BuiltinConst> for Constant {
    fn from(value: BuiltinConst) -> Self {
        match value {
            BuiltinConst::True => Constant::Bool(true),
            BuiltinConst::False => Constant::Bool(false),
            BuiltinConst::Maxint => Constant::Int(i64::MAX),
        }
    }
}

#[derive(Debug, Clone, Copy, EnumIter, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum BuiltinType {
    Integer,
    Real,
    Boolean,
    Char,
    Text,
}

impl Builtin for BuiltinType {
    const DEF_POINT_CONS: fn(Self) -> DefPoint = DefPoint::BuiltinType;
}

#[derive(Debug, Clone, Copy, EnumIter, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum BuiltinVar {
    Input,  // : text
    Output, // : text
}

impl Builtin for BuiltinVar {
    const DEF_POINT_CONS: fn(Self) -> DefPoint = DefPoint::BuiltinVar;
}

#[derive(Debug, Clone, Copy, EnumIter, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum BuiltinFunc {
    // function[T: integer | real] (n: T): T
    Abs,
    Sqr,

    // function (n: integer | real): real
    Sin,
    Cos,
    Exp,
    Ln,
    Sqrt,
    Arctan,

    // function (n: real): integer
    Trunc,
    Round,

    // function[T: Ordinal] (o: T): integer
    Ord,

    // function (integer): char
    Chr,

    // function[T: Ordinal] (o: T): T
    Succ,
    Pred,

    // function (n: integer): bool
    Odd,

    // function (f: file of T): bool
    Eof,

    // function (f: text): bool
    Eoln,
}

impl Builtin for BuiltinProc {
    const DEF_POINT_CONS: fn(Self) -> DefPoint = DefPoint::BuiltinProc;
}

#[derive(Debug, Clone, Copy, EnumIter, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum BuiltinProc {
    // procedure (p: ^T, ...$case_constants?)
    New,
    Dispose,

    // procedure[T] (f: file of T)
    Rewrite,
    Reset,
    Get,
    Put,

    // procedure[...Ts] (f?: text, ...$write_parameters: ...Ts)
    // procedure[T, U] (f?: file of T, ...$write_parameters: T) where T !: file of U
    Write,
    Writeln, // + 0-arg invocation without parens

    // procedure (f?: text, ...$variables: char | integer | real)
    // procedure[T, U] (f?: file of T, ...$variables: T) where T !: file of U
    Read,
    Readln, // + 0-arg invocation without parens

    // procedure[T] (a: array [a_start..a_end: integer] of T, i: integer,
    //               z: packed array [z_start..z_end: integer] of T)
    Pack,
    // procedure[T] (z: packed array [z_start..z_end: integer] of T,
    //               i: integer, a: array [a_start..a_end: integer] of T)
    Unpack,

    // procedure (f: text)
    Page,
}

impl Builtin for BuiltinFunc {
    const DEF_POINT_CONS: fn(Self) -> DefPoint = DefPoint::BuiltinFunc;
}
