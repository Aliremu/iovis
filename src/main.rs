use std::{env, fs};
use std::error::Error;

use clap::Parser;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::{Module, Linkage};
use inkwell::targets::{CodeModel, FileType, RelocMode, Target, TargetMachine};
use inkwell::OptimizationLevel;
use inkwell::values::BasicMetadataValueEnum;
use iovis::compiler::Compiler;
// use iovis::context::Context as iov_Context;
// use iovis::parser::Parser;
// use iovis::lexer::Lexer;
// use iovis::parser::Parser;

type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;
type MulFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_sum(&self) {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0).unwrap().into_int_value();
        let y = function.get_nth_param(1).unwrap().into_int_value();
        let z = function.get_nth_param(2).unwrap().into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum");
        let sum = self.builder.build_int_add(sum, z, "sum");

        self.builder.build_return(Some(&sum));

        // unsafe { self.execution_engine.get_function("sum").ok() }
    }

    fn jit_compile_mul(&self) {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("mul", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0).unwrap().into_int_value();
        let y = function.get_nth_param(1).unwrap().into_int_value();
        let z = function.get_nth_param(2).unwrap().into_int_value();

        let sum = self.builder.build_int_mul(x, y, "mul");
        let sum = self.builder.build_int_mul(sum, z, "mul");

        self.builder.build_return(Some(&sum));

        // unsafe { self.execution_engine.get_function("mul").ok() }
    }

    fn jit_compile_print(&self) {
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], true);
        let function = self.module.add_function("test", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let print = match self.module.get_function("printf") {
            Some(func) => func,
            None => self.module.add_function("printf", fn_type, Some(Linkage::External))
        };

        let arg = self.builder.build_global_string_ptr("Hello World", "val").as_pointer_value();

        let call = self.builder.build_call(print, &[], "call");

        self.builder.build_return(None);
    }

    fn call_print(&self, params: &[BasicMetadataValueEnum]) {
        // let print = self.module.get_function("test").unwrap();
        // let mut args = Vec::new();

        // for param in params {
        //     let val = self.builder.build_global_string_ptr(param, "arg").as_pointer_value().into();
        //     args.push(val);
        // }

        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], true);

        let print = match self.module.get_function("printf") {
            Some(func) => func,
            None => self.module.add_function("printf", fn_type, Some(Linkage::External))
        };

        let sum = self.builder.build_call(print, params, "call");
    }

    fn jit_compile_main(&self) {
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        // let test = self.module.get_function("test").unwrap();

        let i32_type = self.context.i32_type();
        let fn_type = i32_type.fn_type(&[i32_type.into(), i32_type.into()], false);

        let sum = match self.module.get_function("sum") {
            Some(func) => func,
            None => self.module.add_function("sum", fn_type, Some(Linkage::External))
        };

        let out = self.builder.build_call(sum, &[i32_type.const_int(2, false).into(), i32_type.const_int(10, false).into()], "call");

        let hello = self.builder.build_global_string_ptr("hello ", "str").as_pointer_value().into();
        let format = self.builder.build_global_string_ptr("%d\n", "str").as_pointer_value().into();

        self.call_print(&[hello]);
        self.call_print(&[format, out.try_as_basic_value().unwrap_left().into()]);


        let lol = match self.module.get_function("lol") {
            Some(func) => func,
            None => self.module.add_function("lol", self.context.void_type().fn_type(&[], false), Some(Linkage::External))
        };
        let out2 = self.builder.build_call(lol, &[], "call");

        // self.call_print(&[out.into()]);

        // let call = self.builder.build_call(test, &[], "call");

        self.builder.build_return(None);

        // self.builder.build_return(Some(&self.context.i32_type().const_int(0, false)));
    }
}

// fn compile() -> Result<(), Box<dyn Error>> {
//     let context = Context::create();
//     let compiler = Compiler::new(&context);

//     let module = context.create_module("sum");
//     let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
//     let codegen = CodeGen {
//         context: &context,
//         module,
//         builder: context.create_builder(),
//         execution_engine,
//     };

//     // let sum = codegen.jit_compile_sum();

//     let mul = codegen.jit_compile_mul();

//     let print = codegen.jit_compile_print();

//     let main = codegen.jit_compile_main();

//     // let x = 1u64;
//     // let y = 2u64;
//     // let z = 3u64;

//     // unsafe {
//     //     println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
//     //     assert_eq!(sum.call(x, y, z), x + y + z);
//     // }

//     let target_triple = TargetMachine::get_default_triple();
//     let cpu = TargetMachine::get_host_cpu_name().to_string();
//     let features = TargetMachine::get_host_cpu_features().to_string();

//     let target = Target::from_triple(&target_triple).map_err(|e| format!("{:?}", e))?;
//     let target_machine = target
//         .create_target_machine(
//             &target_triple,
//             &cpu,
//             &features,
//             OptimizationLevel::Default,
//             RelocMode::Default,
//             CodeModel::Default,
//         )
//         .ok_or_else(|| "Unable to create target machine!".to_string())?;

//     target_machine
//         .write_to_file(&codegen.module, FileType::Object, "main.iov".as_ref())
//         .map_err(|e| format!("{:?}", e))?;

//     Ok(())
// }

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Place the output into <file>.
    #[arg(short, long, default_value = "out.o")]
    o: String,

    /// Compile and assemble, but do not link.
    #[arg(short, long)]
    c: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();

    let contents = fs::read_to_string(args.c).expect(format!("Failed to read file").as_str());

    // let contents = fs::read_to_string("main.iov").expect(format!("Failed to read file").as_str());

    Compiler::init_targets();

    let context = Context::create();
    let mut compiler = Compiler::new(&context);

    compiler.analyze(contents.clone());
    compiler.compile(contents);
    compiler.write_to_file(&args.o)?;
    // compiler.write_to_file("rewrite.o")?;
    /*
    
     */

    // compile()?;
    // let args: Vec<String> = env::args().collect();
    // match args.get(1) {
    //     Some(file) => {
    //         let contents = fs::read_to_string(file).expect(format!("Failed to read file {}", file).as_str());
    //         let mut parser = Parser::new(contents);
    //         match parser.parse() {
    //             Ok(parsed) => {
    //                 let mut ctx = iov_Context::new();

    //                 for stmt in parsed {
    //                     ctx.evaluate_stmt(stmt);
    //                 }

    //                 print!("{}", ctx.output);
    //             }

    //             Err(err) => {
    //                 print!("{}", err);
    //             }
    //         }
    //     }

    //     None => {
    //         let mut ctx = Context::new();
    //         let stdin = io::stdin();

    //         println!("Iovis Interpreter");

    //         loop {
    //             print!("> ");
    //             io::stdout().flush()?;
    //             let mut line = String::new();
    //             stdin.lock().read_line(&mut line)?;

    //             if line.trim() == "quit()" {
    //                 break
    //             }

    //             if line.is_empty() {
    //                 break;
    //             } else {
    //                 let mut parser = Parser::new(line);
    //                 match parser.parse() {
    //                     Ok(parsed) => {
    //                         for stmt in parsed {
    //                             ctx.evaluate_stmt(stmt);
    //                         }

    //                         print!("{}", ctx.flush());
    //                     }

    //                     Err(err) => {
    //                         println!("{}", err);
    //                     }
    //                 }
    //             }
    //         }
    //     }
    // }

    Ok(())
}