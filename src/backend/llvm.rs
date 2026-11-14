use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{
        CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
    },
};

use crate::{tast::program::Program, utils::Spanned};

pub struct LLVMContext<'ctx> {
    pub context: &'ctx Context,
    pub builder: Builder<'ctx>,
    pub module: Module<'ctx>,
    pub machine: TargetMachine,
}

impl<'ctx> LLVMContext<'ctx> {
    pub fn new(context: &'ctx Context, target_str: Option<&str>) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("emil_module");

        let triple = match target_str {
            Some(target_str) => TargetTriple::create(target_str),
            None => {
                Target::initialize_native(&InitializationConfig::default())
                    .expect("failed to initialize native target");
                TargetMachine::get_default_triple()
            }
        };

        let target = Target::from_triple(&triple).expect("Unknown target specified");

        println!("triple: {}", triple.as_str().to_string_lossy());
        println!(
            "target: {} ({})",
            target.get_name().to_string_lossy(),
            target.get_description().to_string_lossy()
        );
        println!("host cpu: {}", TargetMachine::get_host_cpu_name().to_string());
        println!("cpu features: {}", TargetMachine::get_host_cpu_features().to_string());

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        Self { context, builder, module, machine }
    }

    pub fn dump_module(&self) {
        println!("\nLLVM IR:\n{}", self.module.print_to_string().to_string());
    }

    pub fn dump_assembly(&self) {
        let buf = self
            .machine
            .write_to_memory_buffer(&self.module, FileType::Assembly)
            .expect("Failed to write asm");

        println!("\nAsm Repr:\n{}", str::from_utf8(buf.as_slice()).unwrap());
    }

    pub fn compile_program(&self, program: &Program) {
        let param_types = &[];
        let program_type = self.context.void_type().fn_type(param_types, false);
        let program = self.module.add_function("__program", program_type, None);
        let basic_block = self.context.append_basic_block(program, "entry");
        self.builder.position_at_end(basic_block);
        self.builder.build_return(None).unwrap();
    }
}
