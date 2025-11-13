use lasso::Spur;

use crate::utils::Spanned;

pub mod expr;
pub mod program;
pub mod stmt;

pub type Ident = Spanned<Spur>;
pub type UnspanIdent = Spur;
