use std::ops;
use crate::lexer::{Span, Literal};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,

    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    PowEq,

    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    And,
    Or
}

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum UnaryOp {
    Not,
    Neg,
}

// #[derive(Debug, PartialEq, PartialOrd, Clone, Serialize, Deserialize)]
// pub enum Literal {
//     Integer(i64),
//     Decimal(f64),
//     String(String),
//     Boolean(bool)
// }

// impl From<String> for Literal {
//     fn from(other: String) -> Literal {
//         Literal::String(other)
//     }
// }

// impl<'a> From<&'a str> for Literal {
//     fn from(other: &'a str) -> Literal {
//         Literal::String(other.to_string())
//     }
// }

// impl From<i64> for Literal {
//     fn from(other: i64) -> Literal {
//         Literal::Integer(other)
//     }
// }

// impl From<f64> for Literal {
//     fn from(other: f64) -> Literal {
//         Literal::Decimal(other)
//     }
// }

// impl ops::Add<i64> for Literal {
//     type Output = Literal;

//     fn add(self, _rhs: i64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(n + _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n + _rhs as f64),
//             Literal::String(n)  => { 
//                 let mut clone = n.to_owned(); 
//                 clone.push_str(_rhs.to_string().as_str()); 
//                 println!("{:?}", clone);
//                 Literal::String(clone) 
//             },
//             _ => todo!()
//         }
//     }
// }

// impl ops::Add<f64> for Literal {
//     type Output = Literal;

//     fn add(self, _rhs: f64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Decimal(n as f64 + _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n + _rhs),
//             Literal::String(n)  => { 
//                 let mut clone = n.to_owned(); 
//                 clone.push_str(_rhs.to_string().as_str()); 
//                 Literal::String(clone) 
//             },
//             _ => todo!()
//         }
//     }
// }

// impl ops::Add<String> for Literal {
//     type Output = Literal;

//     fn add(self, _rhs: String) -> Literal {
//         match self {
//             Literal::Integer(n) => {
//                 let mut clone = n.to_string().to_owned(); 
//                 clone.push_str(_rhs.as_str()); 
//                 Literal::String(clone) 
//             },
//             Literal::Decimal(n) => {
//                 let mut clone = n.to_string().to_owned(); 
//                 clone.push_str(_rhs.as_str()); 
//                 Literal::String(clone) 
//             },
//             Literal::String(n)  => { 
//                 let mut clone = n.to_owned(); 
//                 clone.push_str(_rhs.as_str()); 
//                 Literal::String(clone) 
//             },
//             _ => todo!()
//         }
//     }
// }

// impl<'a> ops::Add<&'a str> for Literal {
//     type Output = Literal;

//     fn add(self, _rhs: &'a str) -> Literal {
//         match self {
//             Literal::Integer(n) => {
//                 let mut clone = n.to_string().to_owned(); 
//                 clone.push_str(_rhs); 
//                 Literal::String(clone) 
//             },
//             Literal::Decimal(n) => {
//                 let mut clone = n.to_string().to_owned(); 
//                 clone.push_str(_rhs); 
//                 Literal::String(clone) 
//             },
//             Literal::String(n)  => { 
//                 let mut clone = n.to_owned(); 
//                 clone.push_str(_rhs); 
//                 Literal::String(clone) 
//             },
//             _ => todo!()
//         }
//     }
// }

// impl ops::Sub<i64> for Literal {
//     type Output = Literal;

//     fn sub(self, _rhs: i64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(n - _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n - _rhs as f64),
//             _ => todo!()
//         }
//     }
// }

// impl ops::Sub<f64> for Literal {
//     type Output = Literal;

//     fn sub(self, _rhs: f64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Decimal(n as f64 - _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n - _rhs),
//             _ => todo!()
//         }
//     }
// }

// impl ops::Mul<i64> for Literal {
//     type Output = Literal;

//     fn mul(self, _rhs: i64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(n * _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n * _rhs as f64),
//             Literal::String(n)  => {
//                 let mut base = String::new();

//                 for _ in 0.._rhs {
//                     base.push_str(&n); 
//                 }
                
//                 Literal::String(base) 
//             },
//             _ => todo!()
//         }
//     }
// }

// impl ops::Mul<f64> for Literal {
//     type Output = Literal;

//     fn mul(self, _rhs: f64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Decimal(n as f64 * _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n * _rhs),
//             _ => todo!()
//         }
//     }
// }

// impl ops::Mul<String> for Literal {
//     type Output = Literal;

//     fn mul(self, _rhs: String) -> Literal {
//         match self {
//             Literal::Integer(n) => {
//                 let mut base = String::new();

//                 for _ in 0..n {
//                     base.push_str(&_rhs); 
//                 }
                
//                 Literal::String(base) 
//             },
//             _ => todo!()
//         }
//     }
// }

// impl<'a> ops::Mul<&'a str> for Literal {
//     type Output = Literal;

//     fn mul(self, _rhs: &'a str) -> Literal {
//         match self {
//             Literal::Integer(n) => {
//                 let mut base = String::new();

//                 for _ in 0..n {
//                     base.push_str(_rhs); 
//                 }
                
//                 Literal::String(base) 
//             },
//             _ => todo!()
//         }
//     }
// }

// impl ops::Div<i64> for Literal {
//     type Output = Literal;

//     fn div(self, _rhs: i64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(n / _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n / _rhs as f64),
//             _ => todo!()
//         }
//     }
// }

// impl ops::Div<f64> for Literal {
//     type Output = Literal;

//     fn div(self, _rhs: f64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Decimal(n as f64 / _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n / _rhs),
//             _ => todo!()
//         }
//     }
// }

// impl ops::Rem<i64> for Literal {
//     type Output = Literal;

//     fn rem(self, _rhs: i64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(n % _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n % _rhs as f64),
//             _ => todo!()
//         }
//     }
// }

// impl ops::Rem<f64> for Literal {
//     type Output = Literal;

//     fn rem(self, _rhs: f64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Decimal(n as f64 % _rhs),
//             Literal::Decimal(n) => Literal::Decimal(n % _rhs),
//             _ => todo!()
//         }
//     }
// }

// impl ops::Add<Literal> for Literal {
//     type Output = Literal;

//     fn add(self, _rhs: Literal) -> Literal {
//         match _rhs {
//             Literal::Integer(n) => self + n,
//             Literal::Decimal(n) => self + n,
//             Literal::String(n)  => self + n,
//             _ => todo!()
//         }
//     }
// }

// impl ops::Sub<Literal> for Literal {
//     type Output = Literal;

//     fn sub(self, _rhs: Literal) -> Literal {
//         match _rhs {
//             Literal::Integer(n) => self - n,
//             Literal::Decimal(n) => self - n,
//             _ => todo!()
//         }
//     }
// }


// impl ops::Mul<Literal> for Literal {
//     type Output = Literal;

//     fn mul(self, _rhs: Literal) -> Literal {
//         match _rhs {
//             Literal::Integer(n) => self * n,
//             Literal::Decimal(n) => self * n,
//             Literal::String(n)  => self * n,
//             _ => todo!()
//         }
//     }
// }

// impl ops::Div<Literal> for Literal {
//     type Output = Literal;

//     fn div(self, _rhs: Literal) -> Literal {
//         match _rhs {
//             Literal::Integer(n) => self / n,
//             Literal::Decimal(n) => self / n,
//             _ => todo!()
//         }
//     }
// }

// impl ops::Rem<Literal> for Literal {
//     type Output = Literal;

//     fn rem(self, _rhs: Literal) -> Literal {
//         match _rhs {
//             Literal::Integer(n) => self % n,
//             Literal::Decimal(n) => self % n,
//             _ => todo!()
//         }
//     }
// }

// pub trait Pow<T> {
//     type Output;
//     fn pow(self, rhs: T) -> Self::Output;
// }

// impl Pow<i64> for Literal {
//     type Output = Literal;

//     fn pow(self, _rhs: i64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(n.pow(_rhs.try_into().unwrap())),
//             Literal::Decimal(n) => Literal::Decimal(n.powf(_rhs as f64)),
//             _ => todo!()
//         }
//     }
// }

// impl Pow<f64> for Literal {
//     type Output = Literal;

//     fn pow(self, _rhs: f64) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(n.pow((_rhs as i64).try_into().unwrap())),
//             Literal::Decimal(n) => Literal::Decimal(n.powf(_rhs)),
//             _ => todo!()
//         }
//     }
// }

// impl Pow<Literal> for Literal {
//     type Output = Literal;

//     fn pow(self, _rhs: Literal) -> Literal {
//         match _rhs {
//             Literal::Integer(n) => self.pow(n),
//             Literal::Decimal(n) => self.pow(n),
//             _ => todo!()
//         }
//     }
// }

// pub trait Bang {
//     type Output;
//     fn bang(self) -> Self::Output;
// }

// impl Bang for Literal {
//     type Output = Literal;

//     fn bang(self) -> Literal {
//         match self {
//             Literal::Boolean(n) => Literal::Boolean(!n),
//             _ => todo!()
//         }
//     }
// }

// pub trait Sub {
//     type Output;
//     fn sub(self) -> Self::Output;
// }

// impl Sub for Literal {
//     type Output = Literal;

//     fn sub(self) -> Literal {
//         match self {
//             Literal::Integer(n) => Literal::Integer(-n),
//             Literal::Decimal(n) => Literal::Decimal(-n),
//             _ => todo!()
//         }
//     }
// }

// impl ToString for Literal {
//     fn to_string(&self) -> String {
//         match self {
//             Literal::Boolean(n) => n.to_string(),
//             Literal::Integer(n) => n.to_string(),
//             Literal::Decimal(n) => n.to_string(),
//             Literal::String(n)  => n.to_string(),
//         }
//     }
// }

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct LocalDeclaration {
    pub ident: String, 
    pub ty: Option<TyKind>,
    pub value: Option<Expr>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Type {
    pub ident: String
}

// TODO
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum TyKind {
    Reg(String),
    Reference(Box<TyKind>),
    Array(String),

    Void
}

impl TyKind {
    pub fn is_reference(&self) -> bool {
        matches!(self, Self::Reference(_))
    }

    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub fn get_ident(&self) -> String {
        match self {
            Self::Array(n)     => n.to_string(),
            Self::Reference(n) => (*n).get_ident(),
            Self::Reg(n)       => n.to_string(),
            Self::Void         => "void".to_string()
        }
    }
}

// TODO: References, sized arrays, ...
#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct NamedParameter {
    pub ident: String, 
    pub ty: TyKind
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Parameter {
    Variadic,
    NamedParameter(NamedParameter)
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FunctionDeclaration {
    pub ident: String, 
    pub params: Vec<Parameter>,
    pub output: TyKind,
    pub block: Block
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Return {
    pub ret: Option<Expr>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct If {
    pub cond: Expr,
    pub then_branch: Block,
    pub else_branch: Option<Block>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct While {
    pub cond: Expr,
    pub block: Block
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Field {
    pub ident: String,
    pub ty: String
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Struct {
    pub ident: String,
    pub fields: Vec<Field>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum StmtKind {
    LocalDeclaration(LocalDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    Block(Block),
    Semi(Expr),
    Return(Return),

    ForeignFn(String, Vec<Parameter>, TyKind),

    Import(String),
    Extern(Block),

    Struct(Struct),

    While(While),
    If(If)
}

impl Into<String> for Expr {
    fn into(self) -> String {
        match self.kind {
            ExprKind::IdentExpr(n) => n,
            _ => panic!()
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct BinaryExpr {
    pub op: BinaryOp, 
    pub left: Box<Expr>, 
    pub right: Box<Expr>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub op: UnaryOp, 
    pub expr: Box<Expr>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ArrExpr {
    pub elems: Vec<Expr>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct IndexExpr {
    pub expr: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct AssignExpr {
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct FunctionCallExpr {
    pub func: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct MethodCallExpr {
    pub receiver: String,
    pub method: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct MemberExpr {
    pub receiver: String,
    pub member: String
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum ExprKind {
    LiteralExpr(Literal),
    IdentExpr(String),
    ReferenceExpr(String),
    ArrExpr(ArrExpr),
    IndexExpr(IndexExpr),
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    AssignExpr(AssignExpr),
    BlockExpr(Block),
    MemberExpr(MemberExpr),
    // TODO: String ?
    FunctionCallExpr(FunctionCallExpr),
    MethodCallExpr(MethodCallExpr),
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span
}