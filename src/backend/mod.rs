use std::hint::black_box;

use inkwell::context::Context;

use crate::{backend::llvm::LLVMContext, tast::program::Program};

mod llvm;

pub fn compile_llvm<'src>(program: &'src Program) {
    let context = Context::create();
    let compiler = LLVMContext::new(&context, None);
    black_box(compiler);
}
