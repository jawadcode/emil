use inkwell::context::Context;

use crate::{backend::llvm::LLVMContext, tast::program::Program};

mod llvm;

pub fn compile_llvm(program: &Program) {
    let context = Context::create();
    let compiler = LLVMContext::new(&context, None);
    compiler.compile_program(program);
    compiler.dump_module();
    compiler.dump_assembly();
}
