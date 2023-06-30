use std::{error::Error, io::Result, any::Any, collections::HashMap, ops::DerefMut, fs};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Module, Linkage},
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine, InitializationConfig, TargetData},
    values::{IntValue, BasicValueEnum, BasicValue, FloatValue, InstructionOpcode, FunctionValue, BasicMetadataValueEnum, InstructionValue},
    OptimizationLevel, types::{BasicMetadataTypeEnum, IntType, ArrayType, FunctionType, FloatType, VoidType, BasicTypeEnum, PointerType, BasicType, AnyType, AnyTypeEnum}, AddressSpace, IntPredicate, FloatPredicate, passes::{PassManager, PassManagerBuilder},
};

use crate::{
    ast::{BinaryOp, Expr, Stmt, BinaryExpr, UnaryExpr, UnaryOp, FunctionCallExpr, FunctionDeclaration, Parameter, NamedParameter, StmtKind, ExprKind, If, While, Struct, MemberExpr, AssignExpr, TyKind, Block},
    parser::Parser, lexer::Literal,
};

use unescape::unescape;

trait Add<'ctx, Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Add<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn add(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_add(val, rhs, "tmpadd").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_add(val, conv, "tmpadd").into()
            }
            _ => panic!("")
        }
    }
}

impl<'ctx> Add<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn add(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_add(conv, rhs, "tmpadd").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_add(val, rhs, "tmpadd").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Add<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn add(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.add(val, compiler),
            BasicValueEnum::FloatValue(val) => self.add(val, compiler),
            _ => panic!("")
        }
    }
}

trait Sub<'ctx, Rhs = Self> {
    type Output;

    fn sub(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Sub<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn sub(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_sub(val, rhs, "tmpsub").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();
                
                compiler.builder.build_float_sub(val, conv, "tmpsub").into()
            }
            _ => panic!("")
        }
    }


}

impl<'ctx> Sub<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn sub(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_sub(conv, rhs, "tmpsub").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_sub(val, rhs, "tmpsub").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Sub<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn sub(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.sub(val, compiler),
            BasicValueEnum::FloatValue(val) => self.sub(val, compiler),
            _ => panic!("")
        }
    }
}

trait Mul<'ctx, Rhs = Self> {
    type Output;

    fn mul(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Mul<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn mul(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_mul(val, rhs, "tmpmul").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_mul(val, conv, "tmpmul").into()
            }
            _ => panic!("")
        }
    }
}

impl<'ctx> Mul<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn mul(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_mul(conv, rhs, "tmpmul").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_mul(val, rhs, "tmpmul").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Mul<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn mul(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.mul(val, compiler),
            BasicValueEnum::FloatValue(val) => self.mul(val, compiler),
            _ => panic!("")
        }
    }
}

trait Div<'ctx, Rhs = Self> {
    type Output;

    fn div(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Div<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn div(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_signed_div(val, rhs, "tmpdiv").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_div(val, conv, "tmpdiv").into()
            }
            _ => panic!("")
        }
    }
}

impl<'ctx> Div<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn div(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_div(conv, rhs, "tmpdiv").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_div(val, rhs, "tmpdiv").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Div<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn div(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.div(val, compiler),
            BasicValueEnum::FloatValue(val) => self.div(val, compiler),
            _ => panic!("")
        }
    }
}

trait Neg<'ctx> {
    type Output;

    fn neg(self, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Neg<'ctx> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;

    fn neg(self, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_neg(val, "neg").into(),
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_neg(val, "neg").into(),
            _ => panic!("")
        }
    }
}

trait Not<'ctx> {
    type Output;

    fn not(self, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Not<'ctx> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;

    fn not(self, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_neg(val, "neg").into(),
            _ => panic!("")
        }
    }
}

trait Gt<'ctx, Rhs = Self> {
    type Output;

    fn gt(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Gt<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn gt(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_compare(IntPredicate::SGT, val, rhs, "tmpgt").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::OGT, val, conv, "tmpgt").into()
            }
            _ => panic!("{:?}", self)
        }
    }
}

impl<'ctx> Gt<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn gt(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::OGT, conv, rhs, "tmpgt").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_compare(FloatPredicate::OGT, val, rhs, "tmpgt").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Gt<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn gt(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.gt(val, compiler),
            BasicValueEnum::FloatValue(val) => self.gt(val, compiler),
            _ => panic!("")
        }
    }
}

trait Lt<'ctx, Rhs = Self> {
    type Output;

    fn lt(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Lt<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn lt(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_compare(IntPredicate::SLT, val, rhs, "tmplt").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::OLT, val, conv, "tmplt").into()
            }
            _ => panic!("{:?}", self)
        }
    }
}

impl<'ctx> Lt<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn lt(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::OLT, conv, rhs, "tmplt").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_compare(FloatPredicate::OLT, val, rhs, "tmplt").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Lt<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn lt(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.lt(val, compiler),
            BasicValueEnum::FloatValue(val) => self.lt(val, compiler),
            _ => panic!("")
        }
    }
}

trait Ne<'ctx, Rhs = Self> {
    type Output;

    fn ne(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Ne<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn ne(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_compare(IntPredicate::NE, val, rhs, "tmpne").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::ONE, val, conv, "tmpne").into()
            }
            _ => panic!("{:?}", self)
        }
    }
}

impl<'ctx> Ne<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn ne(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::ONE, conv, rhs, "tmpne").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_compare(FloatPredicate::ONE, val, rhs, "tmpne").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Ne<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn ne(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.ne(val, compiler),
            BasicValueEnum::FloatValue(val) => self.ne(val, compiler),
            _ => panic!("")
        }
    }
}


trait Eq<'ctx, Rhs = Self> {
    type Output;

    fn eq(self, rhs: Rhs, compiler: &Compiler<'ctx>) -> Self::Output;
}

impl<'ctx> Eq<'ctx, IntValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn eq(self, rhs: IntValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => compiler.builder.build_int_compare(IntPredicate::EQ, val, rhs, "tmpeq").into(),
            BasicValueEnum::FloatValue(val) => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, rhs, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::OEQ, val, conv, "tmpeq").into()
            }
            _ => panic!("{:?}", self)
        }
    }
}

impl<'ctx> Eq<'ctx, FloatValue<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn eq(self, rhs: FloatValue<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match self {
            BasicValueEnum::IntValue(val)   => {
                let conv = compiler.builder.build_cast(InstructionOpcode::SIToFP, val, compiler.context.f64_type(), "conv").into_float_value();

                compiler.builder.build_float_compare(FloatPredicate::OEQ, conv, rhs, "tmpeq").into()
            }
            BasicValueEnum::FloatValue(val) => compiler.builder.build_float_compare(FloatPredicate::OEQ, val, rhs, "tmpeq").into(),

            _ => panic!("")
        }
    }
}

impl<'ctx> Eq<'ctx, BasicValueEnum<'ctx>> for BasicValueEnum<'ctx> {
    type Output = BasicValueEnum<'ctx>;
    
    fn eq(self, rhs: BasicValueEnum<'ctx>, compiler: &Compiler<'ctx>) -> Self::Output {
        match rhs {
            BasicValueEnum::IntValue(val)   => self.eq(val, compiler),
            BasicValueEnum::FloatValue(val) => self.eq(val, compiler),
            _ => panic!("")
        }
    }
}


#[derive(Clone, Copy)]
pub enum BuiltinType {
    Bool,
    Char,
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    String,
    Void
}

impl BuiltinType {
    pub const ALL: &'static [(&'static str, BuiltinType)] = &[
        ("void", BuiltinType::Void),
        ("char", BuiltinType::Char),
        ("bool", BuiltinType::Bool),
        ("string",  BuiltinType::String),

        ("i8",    BuiltinType::I8),
        ("i16",   BuiltinType::I16),
        ("i32",   BuiltinType::I32),
        ("i64",   BuiltinType::I64),
        ("i128",  BuiltinType::I128),

        ("usize", BuiltinType::Usize),
        ("u8",    BuiltinType::U8),
        ("u16",   BuiltinType::U16),
        ("u32",   BuiltinType::U32),
        ("u64",   BuiltinType::U64),
        ("u128",  BuiltinType::U128),

        ("f32", BuiltinType::F32),
        ("f64", BuiltinType::F64),
    ];

    pub fn by_name(name: &str) -> Option<Self> {
        Self::ALL.iter().find_map(|(n, ty)| if n == &name { Some(*ty) } else { None })
    }
}

pub struct Compiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    symbols: HashMap<String, (BasicTypeEnum<'ctx>, BasicValueEnum<'ctx>)>,
    functions: HashMap<String, FunctionValue<'ctx>>,

    type_bool: IntType<'ctx>,
    type_char: IntType<'ctx>,

    type_usize: IntType<'ctx>,
    type_u8: IntType<'ctx>,
    type_u16: IntType<'ctx>,
    type_u32: IntType<'ctx>,
    type_u64: IntType<'ctx>,
    type_u128: IntType<'ctx>,

    type_i8: IntType<'ctx>,
    type_i16: IntType<'ctx>,
    type_i32: IntType<'ctx>,
    type_i64: IntType<'ctx>,
    type_i128: IntType<'ctx>,

    type_f32: FloatType<'ctx>,
    type_f64: FloatType<'ctx>,

    type_str: PointerType<'ctx>,
    type_void: VoidType<'ctx>,

    cur_function: Option<FunctionValue<'ctx>>
    // execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> Compiler<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("iovis");
        let builder = context.create_builder();

        let mut compiler = Compiler {
            context,
            module,
            builder,
            symbols: HashMap::new(),
            functions: HashMap::new(),

            type_bool: context.bool_type(),
            type_char: context.i8_type(),

            type_usize: context.i64_type(),
            type_u8:    context.i8_type(),
            type_u16:   context.i16_type(),
            type_u32:   context.i32_type(),
            type_u64:   context.i64_type(),
            type_u128:  context.i128_type(),

            type_i8:    context.i8_type(),
            type_i16:   context.i16_type(),
            type_i32:   context.i32_type(),
            type_i64:   context.i64_type(),
            type_i128:  context.i128_type(),

            type_f32:   context.f32_type(),
            type_f64:   context.f64_type(),

            type_void:  context.void_type(),
            type_str:   context.i8_type().ptr_type(AddressSpace::default()),

            cur_function: None
        };

        // let void_type = context.void_type();
        // let fn_type = void_type.fn_type(&[], true);

        // let print = match compiler.module.get_function("printf") {
        //     Some(func) => func,
        //     None => compiler.module.add_function("printf", fn_type, Some(Linkage::External))
        // };

        compiler
    }

    // fn get_builtin_function(ident: &str) -> FunctionValue<'ctx> {

    // }

    pub fn init_targets() {
        Target::initialize_all(&InitializationConfig::default());
    }

    fn compile_unary_expr(&mut self, node: UnaryExpr) -> Result<BasicValueEnum<'ctx>> {
        let expr = self.compile_expr(*node.expr)?;

        let out = match node.op {
            UnaryOp::Neg => expr.neg(self),
            UnaryOp::Not => expr.not(self),
            _ => panic!("NOT IMPLEMENTED BINARY EXPR"),
        };

        Ok(out)
    }

    fn compile_binary_expr(&mut self, node: BinaryExpr) -> Result<BasicValueEnum<'ctx>> {
        let left = self.compile_expr(*node.left)?;
        let right = self.compile_expr(*node.right)?;

        let out = match node.op {
            BinaryOp::Add => left.add(right, self),
            BinaryOp::Sub => left.sub(right, self),
            BinaryOp::Mul => left.mul(right, self),
            BinaryOp::Div => left.div(right, self),
            BinaryOp::Eq  => left.eq(right, self),
            BinaryOp::Ne  => left.ne(right, self),
            BinaryOp::Gt  => left.gt(right, self),
            BinaryOp::Lt  => left.lt(right, self),
            // BinaryOp::Mod => Ok(self.builder.build_int_signed_rem(left, right, "tmprem")),
            n => panic!("NOT IMPLEMENTED BINARY EXPR {:?}", n),
        };

        Ok(out)
    }

    fn compile_lit(&self, expr: Literal) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Literal::Boolean(n) => return Ok(self.context.bool_type().const_int(n as u64, false).into()),
            Literal::Integer(n) => return Ok(self.context.i32_type().const_int(n as u64, true).into()),
            Literal::Decimal(n) => return Ok(self.context.f32_type().const_float(n).into()),
            Literal::String(n)  => return Ok(self.builder.build_global_string_ptr(&unescape(&n).unwrap(), "str").as_pointer_value().into())
        }
    }

    fn compile_function_call(&mut self, expr: FunctionCallExpr) -> Result<BasicValueEnum<'ctx>> {
        let func = self.functions.get(&expr.func).unwrap().clone();
        let args = expr.args.into_iter().map(|x| self.compile_expr(x).unwrap().into()).collect::<Vec<BasicMetadataValueEnum<'ctx>>>();
  
        // let mut va_args = Vec::new();

        // for (index, arg) in args.iter().enumerate() {
        //     if index >= func.count_params() as usize {
        //         va_args.push(arg.into_array_value());
        //     }
        // }

        // let type_ptr = self.type_u64.ptr_type(AddressSpace::default());
        // let type_arr = type_ptr.array_type(va_args.len() as u32);
        // let array = type_arr.const_array(va_args.as_slice());
        // let ptr = self.builder.build_bitcast(array, type_ptr, "conv");

        // if !va_args.is_empty() {
        //     self.symbols.insert("va_args".to_string(), self.builder.build_va_arg(ptr.into_pointer_value(), type_ptr, "va_args"));
        // }


        // let args = func.get_params().into_iter().map(|x| x.into()).collect::<Vec<BasicMetadataValueEnum<'ctx>>>();

        let return_type = func.get_type().get_return_type();

        let call = self.builder.build_call(func, &args, "call");

        if return_type.is_some() {
            Ok(call.try_as_basic_value().unwrap_left().into())
        } else {
            Ok(self.type_usize.const_zero().into())
        }

        /*let void_type = self.context.void_type();
        let i64_type  = self.context.i64_type();
        let fn_type = void_type.fn_type(&[], true);

        let print = match self.module.get_function(&func) {
            Some(func) => func,
            None => self.module.add_function(&func, fn_type, Some(Linkage::External))
        };

        let args = args.into_iter().map(|x| self.compile_expr(x.val).unwrap()).collect::<Vec<BasicValueEnum<'ctx>>>();

        let call = self.builder.build_call(print, &args, "call");
        
        return Ok(i64_type.const_int(1, true).into());*/
    }

    fn from_builtin(&self, from: BuiltinType) -> BasicTypeEnum<'ctx> {
        match from {
            BuiltinType::Bool   => self.type_bool.into(),
            BuiltinType::Char   => self.type_char.into(),

            BuiltinType::Usize  => self.type_usize.into(),
            BuiltinType::U8     => self.type_u8.into(),
            BuiltinType::U16    => self.type_u16.into(),
            BuiltinType::U32    => self.type_u32.into(),
            BuiltinType::U64    => self.type_u64.into(),
            BuiltinType::U128   => self.type_u128.into(),

            BuiltinType::I8     => self.type_i8.into(),
            BuiltinType::I16    => self.type_i16.into(),
            BuiltinType::I32    => self.type_i32.into(),
            BuiltinType::I64    => self.type_i64.into(),
            BuiltinType::I128   => self.type_i128.into(),

            BuiltinType::F32    => self.type_f32.into(),
            BuiltinType::F64    => self.type_f64.into(),

            BuiltinType::String => self.type_str.into(),
            _                   => panic!()
        }
    }

    fn compile_expr(&mut self, expr: Expr) -> Result<BasicValueEnum<'ctx>> {
        match expr.kind {
            ExprKind::LiteralExpr(lit)       => self.compile_lit(lit),
            ExprKind::UnaryExpr(expr)        => self.compile_unary_expr(expr),
            ExprKind::BinaryExpr(expr)       => self.compile_binary_expr(expr),
            ExprKind::FunctionCallExpr(call) => self.compile_function_call(call),
            ExprKind::IdentExpr(ident)       => {
                if let Some(func) = self.cur_function {
                    let p = func.get_params().iter().copied().find(|x| {
                        let name = match &x {
                            BasicValueEnum::ArrayValue(n) => n.get_name(),
                            BasicValueEnum::FloatValue(n) => n.get_name(),
                            BasicValueEnum::IntValue(n) => n.get_name(),
                            BasicValueEnum::PointerValue(n) => n.get_name(),
                            _ => panic!()
                        };

                        name.to_str().unwrap() == ident
                    });

                    if p.is_some() {
                        return Ok(p.unwrap());
                    } else {
                        let ptr = self.symbols.get(&ident).expect(&format!("{:?}", expr.span));

                        //TODO: UGLY
                        /*let load = match ptr.0 {
                            BasicMetadataTypeEnum::IntType(_) => self.builder.build_load(self.type_i32, ptr.1.into_pointer_value(), &ident),
                            BasicMetadataTypeEnum::FloatType(_) => self.builder.build_load(self.type_f32, ptr.1.into_pointer_value(), &ident),
                            BasicMetadataTypeEnum::PointerType(_) => self.builder.build_load(self.type_u8.ptr_type(AddressSpace::default()), ptr.1.into_pointer_value(), &ident),
                            BasicMetadataTypeEnum::ArrayType(_) => self.builder.build_load(self.type_u8.array_type(56), ptr.1.into_pointer_value(), &ident),
                            _ => panic!("wtf mnan")
                        };*/

                        if ptr.0.is_pointer_type() {
                            //return Ok((*ptr).1);
                        }

                        let load = self.builder.build_load(ptr.0, ptr.1.into_pointer_value(), &ident);
                        // let load = self.builder.build_load(self.type_i8, ptr.into_pointer_value(), &ident);
                        println!("Ident Get {:?}", load.get_type());

                        // 
                        
                        return Ok(load);
                    }
                } else {
                    panic!();
                }
                // println!("{:?} {:?}", ident, self.symbols);
                // Ok(*self.symbols.get(&ident).unwrap())
            },

            ExprKind::ReferenceExpr(ident)       => {
                if let Some(func) = self.cur_function {
                    let p = func.get_params().iter().copied().find(|x| {
                        let name = match &x {
                            BasicValueEnum::ArrayValue(n) => n.get_name(),
                            BasicValueEnum::FloatValue(n) => n.get_name(),
                            BasicValueEnum::IntValue(n) => n.get_name(),
                            BasicValueEnum::PointerValue(n) => n.get_name(),
                            _ => panic!()
                        };

                        name.to_str().unwrap() == ident
                    });

                    if p.is_some() {
                        return Ok(p.unwrap());
                    } else {
                        let ptr = self.symbols.get(&ident).unwrap();
                        println!("Ref Get {:?}", ptr);

                        return Ok((*ptr).1);
                    }
                } else {
                    panic!();
                }
                // println!("{:?} {:?}", ident, self.symbols);
                // Ok(*self.symbols.get(&ident).unwrap())
            },

            ExprKind::MemberExpr(MemberExpr { receiver, member }) => {
                let var = self.symbols.get(&receiver).unwrap();
                let ptr = var.1;
                let ty  = var.0;

                let load = self.builder.build_load(self.type_u32, ptr.into_pointer_value(), &receiver);
                Ok(load)
                // self.context.get_struct_type()
                // Ok(self.builder.build_struct_gep(ty, ptr.into_pointer_value(), 0, "tmpgep").unwrap().into())
            },

            ExprKind::AssignExpr(AssignExpr { left, right }) => {
                let left: String = (*left).into();
                let val = self.compile_expr(*right)?;
                let ptr = self.symbols.get(&left).unwrap();

                let load = self.builder.build_store(ptr.1.into_pointer_value(), val);

                Ok(val)
            }

            _ => panic!("WTF {:?}", expr)
        }
    }

    fn type_from_kind(&self, ty: &TyKind) -> BasicTypeEnum<'ctx> {
        let ret = if let Some(builtin) = BuiltinType::by_name(&ty.get_ident()) {
            self.from_builtin(builtin)
        } else {
            self.context.get_struct_type(&ty.get_ident()).unwrap().into()
        };

        if ty.is_reference() {
            ret.ptr_type(AddressSpace::default()).into()
        } else {
            ret
        }
    }

    fn compile_function(&mut self, func: FunctionDeclaration) {

        let mut params = Vec::new();

        let variadic = func.params.iter().any(|x| *x == Parameter::Variadic);

        for param in &func.params {
            if let Parameter::NamedParameter(NamedParameter { ident, ty }) = param {
                params.push(self.type_from_kind(ty).into());
            }
        }

        if func.output.is_void() {
            self.type_void.fn_type(params.as_slice(), variadic)
        } else {
            let ty: BasicTypeEnum = match BuiltinType::by_name(&func.output.get_ident()) {
                Some(n) => self.from_builtin(n),
                None => self.context.get_struct_type(&func.output.get_ident()).unwrap().into(),
            };

            if func.output.is_reference() {
                ty.ptr_type(AddressSpace::default()).fn_type(params.as_slice(), variadic)
            } else {
                ty.fn_type(params.as_slice(), variadic)
            }
        };

        let function = *self.functions.get(&func.ident).unwrap(); //self.module.add_function(&func.ident, fn_type, None);

        self.cur_function.insert(function);
        
        self.functions.insert(func.ident, function);
        // self.builder.build_va_arg(list, type_, name)

        for (index, param) in func.params.iter().enumerate() {
            if let Parameter::NamedParameter(NamedParameter { ident, ty }) = param {
                function.get_nth_param(index as u32).unwrap().set_name(&ident);
                // self.symbols.insert(param.ident.val.clone(), function.get_nth_param(i).unwrap().into());
            }
        }

        // TODO
        // println!("{:?}", self.functions);

        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        for stmt in func.block.stmts {
            self.compile_stmt(stmt);
        }

        if func.output.is_void() {
            self.builder.build_return(None);
        } else {
            // self.builder.build_return(Some(&self.type_usize.const_int(1, true)));
        }

        self.cur_function = None;
    }

    fn declare_function(&mut self, ident: String, params: Vec<Parameter>, output: TyKind) {
        let str_type  = self.context.i8_type().ptr_type(AddressSpace::default());
        let void_type = self.context.void_type();

        let test = params;

        let mut params = Vec::new();

        let variadic = test.iter().any(|x| *x == Parameter::Variadic);

        for param in &test {
            if let Parameter::NamedParameter(NamedParameter { ident, ty }) = param {
                // let mut param = self.from_builtin(BuiltinType::by_name(&ty).unwrap());

                let mut param: BasicTypeEnum<'ctx> = {
                    if let Some(type_builtin) = BuiltinType::by_name(&ty.get_ident()) {
                        self.from_builtin(type_builtin)
                    } else {
                        println!("declare_function wtf?");
                        self.context.get_struct_type(&ty.get_ident()).unwrap().into()
                    }
                };

                if ty.is_reference() {
                    param = param.ptr_type(AddressSpace::default()).into();
                }

                params.push(param.into());
            }
        }

        //TODO
        // println!("{:?}", params);

        if variadic {
            params.clear();
        }

        let fn_type = if output.is_void() {
            self.type_void.fn_type(params.as_slice(), variadic)
        } else {
            let ty: BasicTypeEnum = match BuiltinType::by_name(&output.get_ident()) {
                Some(n) => self.from_builtin(n),
                None => self.context.get_struct_type(&output.get_ident()).unwrap().into(),
            };

            if output.is_reference() {
                ty.ptr_type(AddressSpace::default()).fn_type(params.as_slice(), variadic)
            } else {
                ty.fn_type(params.as_slice(), variadic)
            }
        };

        let function = match self.module.get_function(&ident) {
            Some(func) => func,
            None => self.module.add_function(&ident, fn_type, Some(Linkage::External))
        };
        
        self.functions.insert(ident, function);
    }

    fn build_extern(&mut self, block: Block) {
        for stmt in block.stmts {
            if let StmtKind::ForeignFn(ident, params, output) = stmt.kind {
                self.declare_function(ident, params, output);
            }
        }
    }

    fn compile_stmt(&mut self, stmt: Stmt) {
        match stmt.kind {
            StmtKind::Semi(expr) => {
                self.compile_expr(expr).unwrap();
            },

            StmtKind::LocalDeclaration(stmt) => {
                let current = self.builder.get_insert_block().unwrap();
                let start = self.cur_function.unwrap().get_first_basic_block().unwrap();
                
                let mut val = None;

                if stmt.value.is_none() && stmt.ty.is_none() {
                    panic!("Variable with ambiguous type!");
                }

                let ty = match stmt.value {
                    Some(v) => {
                        val.insert(self.compile_expr(v).unwrap());
                        val.unwrap().get_type()
                    },

                    None => {
                        let ty = stmt.ty.as_ref().unwrap();

                        if let Some(builtin) = BuiltinType::by_name(&ty.get_ident()) {
                            self.from_builtin(builtin)
                        } else {
                            self.context.get_struct_type(&ty.get_ident()).unwrap().into()
                        }
                    }
                };

                println!("Local Decl {:?}", ty);

                if let Some(inst) = start.get_first_instruction() {
                    self.builder.position_before(&inst);
                } else {
                    self.builder.position_at_end(current);
                }

                let ptr = self.builder.build_alloca(ty, &stmt.ident);
                self.builder.position_at_end(current);

                if val.is_some() {
                    self.builder.build_store(ptr, val.unwrap());
                } else {
                    if stmt.ty.is_some() && stmt.ty.unwrap().get_ident() == "SDL_Rect" {
                        self.builder.build_store(ptr, self.context.get_struct_type("SDL_Rect").unwrap().const_zero());
                    }
                }

                self.symbols.insert(stmt.ident, (ty.into(), ptr.into()));
            }

            StmtKind::FunctionDeclaration(stmt) => {
                self.compile_function(stmt);
            },

            StmtKind::Return(ret) => {
                if let Some(expr) = ret.ret {
                    let val = self.compile_expr(expr).unwrap();
                    self.builder.build_return(Some(&val));
                } else {
                    self.builder.build_return(None);
                }
            },

            StmtKind::Extern(block) => self.build_extern(block),

            StmtKind::If(If { cond, then_branch, else_branch }) => {
                let cond = self.compile_expr(cond).unwrap();
                let start = self.builder.get_insert_block().unwrap();
                let function = start.get_parent().unwrap();
                
                let then_block = self.context.append_basic_block(function, "then");
                let else_block = self.context.append_basic_block(function, "else");
                let merge = self.context.append_basic_block(function, "ifcont");

                self.builder.position_at_end(start);

                // let cmp = self.builder.build_int_compare(IntPredicate::UGT, cond.into_int_value(), self.type_i64.const_all_ones(), "tmpcmp").into();

                self.builder.build_conditional_branch(cond.into_int_value(), then_block, else_block);

                /* IF BRANCH */
                self.builder.position_at_end(then_block);
                
                for stmt in then_branch.stmts {
                    self.compile_stmt(stmt);
                }

                self.builder.build_unconditional_branch(merge);

                let then_block = self.builder.get_insert_block().unwrap();

                /* ELSE BRANCH */

                self.builder.position_at_end(else_block);

                if let Some(block) = else_branch {
                    for stmt in block.stmts {
                        self.compile_stmt(stmt);
                    }
                }

                self.builder.build_unconditional_branch(merge);
                
                let else_block = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(merge);

                let phi = self.builder.build_phi(self.type_i32, "tmpif");
                phi.add_incoming(&[(&self.type_i32.const_zero(), then_block)]);
                phi.add_incoming(&[(&self.type_i32.const_zero(), else_block)]);


                println!("CONDITION {:?}", phi.as_basic_value());
            },

            StmtKind::While(While { cond, block }) => {
                let start = self.builder.get_insert_block().unwrap();
                let function = start.get_parent().unwrap();
                
                let while_block = self.context.append_basic_block(function, "while");
                let body = self.context.append_basic_block(function, "body");
                let exit = self.context.append_basic_block(function, "exit");

                self.builder.position_at_end(start);
                self.builder.build_unconditional_branch(while_block);

                self.builder.position_at_end(while_block);

                let cond = self.compile_expr(cond).unwrap();
                self.builder.build_conditional_branch(cond.into_int_value(), body, exit);

                self.builder.position_at_end(body);

                for stmt in block.stmts {
                    self.compile_stmt(stmt);
                }

                self.builder.build_unconditional_branch(while_block);
                let body = self.builder.get_insert_block().unwrap();

                self.builder.position_at_end(exit);
            },

            StmtKind::Struct(Struct { ident, fields }) => {
                let mut field_types = Vec::new();

                for field in fields {
                    field_types.push(self.from_builtin(BuiltinType::by_name(&field.ty).unwrap()));
                }

                // let type_struct = self.context.opaque_struct_type(&ident);
                self.context.get_struct_type(&ident).unwrap().set_body(field_types.as_slice(), true);

                // println!("STRUCT {:?}", type_struct);
            },

            StmtKind::Import(_) => {},

            _ => panic!("NO STMT YET")
        }
    }

    pub fn compile(&mut self, program: String) {
        let mut parser = Parser::new(program);
        let parsed = parser.parse().unwrap();

        // println!("{:#?}", parsed);

        for stmt in parsed {
            self.compile_stmt(stmt);
        }

        // let void_type = self.context.void_type();
        // let fn_type = void_type.fn_type(&[], false);
        // let function = self.module.add_function("main", fn_type, None);
        // let basic_block = self.context.append_basic_block(function, "entry");

        // self.builder.position_at_end(basic_block);

        // self.builder.build_return(None);

        println!("Compiled successfully");
    }

    pub fn analyze(&mut self, program: String) {
        let mut parser = Parser::new(program);
        let parsed = parser.parse().unwrap();

        println!("{:#?}", parsed);

        for stmt in parsed {
            match stmt.kind {
                StmtKind::FunctionDeclaration(FunctionDeclaration { ident, params, output, block: _ }) => {
                    let str_type  = self.context.i8_type().ptr_type(AddressSpace::default());
                    let void_type = self.context.void_type();
                    let test = params;

                    let mut params = Vec::new();

                    let variadic = test.iter().any(|x| *x == Parameter::Variadic);
            
                    for param in &test {
                        if let Parameter::NamedParameter(NamedParameter { ident, ty }) = param {
                            // let mut param = self.from_builtin(BuiltinType::by_name(&ty).unwrap());

                            let mut param: BasicTypeEnum<'ctx> = {
                                if let Some(type_builtin) = BuiltinType::by_name(&ty.get_ident()) {
                                    self.from_builtin(type_builtin)
                                } else {
                                    println!("FUNCTION DECLARE wtf?");
                                    self.context.get_struct_type(&ty.get_ident()).unwrap().into()
                                }
                            };

                            if ty.is_reference() {
                                param = param.ptr_type(AddressSpace::default()).into();
                            }
            
                            params.push(param.into());
                        }
                    }

                    //TODO
                    // println!("{:?}", params);

                    if variadic {
                        params.clear();
                    }
            
                    let mut is_void = false;
            
                    let fn_type: FunctionType<'ctx> = if output.is_void() {
                            self.type_void.fn_type(params.as_slice(), variadic)
                        } else {
                            let ty: BasicTypeEnum = match BuiltinType::by_name(&output.get_ident()) {
                                Some(n) => match n {
                                    BuiltinType::Bool   => self.type_bool.into(),
                                    BuiltinType::Char   => self.type_char.into(),
                        
                                    BuiltinType::Usize  => self.type_usize.into(),
                                    BuiltinType::U8     => self.type_u8.into(),
                                    BuiltinType::U16    => self.type_u16.into(),
                                    BuiltinType::U32    => self.type_u32.into(),
                                    BuiltinType::U64    => self.type_u64.into(),
                                    BuiltinType::U128   => self.type_u128.into(),
                        
                                    BuiltinType::I8     => self.type_i8.into(),
                                    BuiltinType::I16    => self.type_i16.into(),
                                    BuiltinType::I32    => self.type_i32.into(),
                                    BuiltinType::I64    => self.type_i64.into(),
                                    BuiltinType::I128   => self.type_i128.into(),
                        
                                    BuiltinType::F32    => self.type_f32.into(),
                                    BuiltinType::F64    => self.type_f64.into(),
                        
                                    BuiltinType::String => str_type.into(),
                                    _ => panic!("VOID!")
                                }
                                None => self.context.get_struct_type(&output.get_ident()).unwrap().into(),
                            };

                            if output.is_reference() {
                                ty.ptr_type(AddressSpace::default()).fn_type(params.as_slice(), variadic)
                            } else {
                                ty.fn_type(params.as_slice(), variadic)
                            }
                        };
                    
                    let function = match self.module.get_function(&ident) {
                        Some(func) => func,
                        None => self.module.add_function(&ident, fn_type, None)
                    };
                    
                    self.functions.insert(ident, function);
                },

                StmtKind::Struct(Struct { ident, fields: _ }) => {
                    let type_struct = self.context.opaque_struct_type(&ident);
                },

                StmtKind::Import(file) => {
                    let import = fs::read_to_string(file.clone() + ".iov").expect(format!("Import not found {}", file).as_str());
                    let mut parser = Parser::new(import);
                    let parsed = parser.parse().unwrap();

                    for stmt in parsed {
                        match stmt.kind {
                            StmtKind::Struct(Struct { ident, fields }) => {
                                let type_struct = self.context.opaque_struct_type(&ident);

                                let mut field_types = Vec::new();

                                for field in fields {
                                    field_types.push(self.from_builtin(BuiltinType::by_name(&field.ty).unwrap()));
                                }
            
                                type_struct.set_body(field_types.as_slice(), true);

                                println!("{} {:?}", file, type_struct);
                            },

                            StmtKind::FunctionDeclaration(FunctionDeclaration { ident, params, output, block }) => {
                                self.declare_function(ident, params, output);
                            },

                            StmtKind::Extern(block) => {
                                self.build_extern(block);
                            },

                            _ => {}
                        }
                    }
                },

                _ => {}
            }
        }
    }

    pub fn write_to_file(&mut self, file: &str) -> Result<()> {
        // let pass_manager: PassManager<Module> = PassManager::create(());
        // pass_manager.add_licm_pass();
        // pass_manager.run_on(&self.module);

        self.module.print_to_stderr();

        let target_triple = TargetMachine::get_default_triple();
        let cpu = TargetMachine::get_host_cpu_name().to_string();
        let features = TargetMachine::get_host_cpu_features().to_string();

        let target = Target::from_triple(&target_triple).map_err(|e| format!("{:?}", e)).unwrap();

        

        let target_machine = target
            .create_target_machine(
                &target_triple,
                &cpu,
                &features,
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| "Unable to create target machine!".to_string()).unwrap();

        target_machine
            .write_to_file(&self.module, FileType::Object, file.as_ref())
            .map_err(|e| format!("{:?}", e)).unwrap();

        Ok(())
    }
}
