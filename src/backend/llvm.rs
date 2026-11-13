use inkwell::{
    OptimizationLevel,
    builder::Builder,
    context::Context,
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
};

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
}
