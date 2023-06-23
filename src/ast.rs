use std::ops;
use crate::lexer::Span;

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Bang,
    Sub,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Literal {
    Integer(i64),
    Decimal(f64),
    String(String),
    Boolean(bool)
}

impl From<String> for Literal {
    fn from(other: String) -> Literal {
        Literal::String(other)
    }
}

impl<'a> From<&'a str> for Literal {
    fn from(other: &'a str) -> Literal {
        Literal::String(other.to_string())
    }
}

impl From<i64> for Literal {
    fn from(other: i64) -> Literal {
        Literal::Integer(other)
    }
}

impl From<f64> for Literal {
    fn from(other: f64) -> Literal {
        Literal::Decimal(other)
    }
}

impl ops::Add<i64> for Literal {
    type Output = Literal;

    fn add(self, _rhs: i64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(n + _rhs),
            Literal::Decimal(n) => Literal::Decimal(n + _rhs as f64),
            Literal::String(n)  => { 
                let mut clone = n.to_owned(); 
                clone.push_str(_rhs.to_string().as_str()); 
                println!("{:?}", clone);
                Literal::String(clone) 
            },
            _ => todo!()
        }
    }
}

impl ops::Add<f64> for Literal {
    type Output = Literal;

    fn add(self, _rhs: f64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Decimal(n as f64 + _rhs),
            Literal::Decimal(n) => Literal::Decimal(n + _rhs),
            Literal::String(n)  => { 
                let mut clone = n.to_owned(); 
                clone.push_str(_rhs.to_string().as_str()); 
                Literal::String(clone) 
            },
            _ => todo!()
        }
    }
}

impl ops::Add<String> for Literal {
    type Output = Literal;

    fn add(self, _rhs: String) -> Literal {
        match self {
            Literal::Integer(n) => {
                let mut clone = n.to_string().to_owned(); 
                clone.push_str(_rhs.as_str()); 
                Literal::String(clone) 
            },
            Literal::Decimal(n) => {
                let mut clone = n.to_string().to_owned(); 
                clone.push_str(_rhs.as_str()); 
                Literal::String(clone) 
            },
            Literal::String(n)  => { 
                let mut clone = n.to_owned(); 
                clone.push_str(_rhs.as_str()); 
                Literal::String(clone) 
            },
            _ => todo!()
        }
    }
}

impl<'a> ops::Add<&'a str> for Literal {
    type Output = Literal;

    fn add(self, _rhs: &'a str) -> Literal {
        match self {
            Literal::Integer(n) => {
                let mut clone = n.to_string().to_owned(); 
                clone.push_str(_rhs); 
                Literal::String(clone) 
            },
            Literal::Decimal(n) => {
                let mut clone = n.to_string().to_owned(); 
                clone.push_str(_rhs); 
                Literal::String(clone) 
            },
            Literal::String(n)  => { 
                let mut clone = n.to_owned(); 
                clone.push_str(_rhs); 
                Literal::String(clone) 
            },
            _ => todo!()
        }
    }
}

impl ops::Sub<i64> for Literal {
    type Output = Literal;

    fn sub(self, _rhs: i64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(n - _rhs),
            Literal::Decimal(n) => Literal::Decimal(n - _rhs as f64),
            _ => todo!()
        }
    }
}

impl ops::Sub<f64> for Literal {
    type Output = Literal;

    fn sub(self, _rhs: f64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Decimal(n as f64 - _rhs),
            Literal::Decimal(n) => Literal::Decimal(n - _rhs),
            _ => todo!()
        }
    }
}

impl ops::Mul<i64> for Literal {
    type Output = Literal;

    fn mul(self, _rhs: i64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(n * _rhs),
            Literal::Decimal(n) => Literal::Decimal(n * _rhs as f64),
            Literal::String(n)  => {
                let mut base = String::new();

                for _ in 0.._rhs {
                    base.push_str(&n); 
                }
                
                Literal::String(base) 
            },
            _ => todo!()
        }
    }
}

impl ops::Mul<f64> for Literal {
    type Output = Literal;

    fn mul(self, _rhs: f64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Decimal(n as f64 * _rhs),
            Literal::Decimal(n) => Literal::Decimal(n * _rhs),
            _ => todo!()
        }
    }
}

impl ops::Mul<String> for Literal {
    type Output = Literal;

    fn mul(self, _rhs: String) -> Literal {
        match self {
            Literal::Integer(n) => {
                let mut base = String::new();

                for _ in 0..n {
                    base.push_str(&_rhs); 
                }
                
                Literal::String(base) 
            },
            _ => todo!()
        }
    }
}

impl<'a> ops::Mul<&'a str> for Literal {
    type Output = Literal;

    fn mul(self, _rhs: &'a str) -> Literal {
        match self {
            Literal::Integer(n) => {
                let mut base = String::new();

                for _ in 0..n {
                    base.push_str(_rhs); 
                }
                
                Literal::String(base) 
            },
            _ => todo!()
        }
    }
}

impl ops::Div<i64> for Literal {
    type Output = Literal;

    fn div(self, _rhs: i64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(n / _rhs),
            Literal::Decimal(n) => Literal::Decimal(n / _rhs as f64),
            _ => todo!()
        }
    }
}

impl ops::Div<f64> for Literal {
    type Output = Literal;

    fn div(self, _rhs: f64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Decimal(n as f64 / _rhs),
            Literal::Decimal(n) => Literal::Decimal(n / _rhs),
            _ => todo!()
        }
    }
}

impl ops::Rem<i64> for Literal {
    type Output = Literal;

    fn rem(self, _rhs: i64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(n % _rhs),
            Literal::Decimal(n) => Literal::Decimal(n % _rhs as f64),
            _ => todo!()
        }
    }
}

impl ops::Rem<f64> for Literal {
    type Output = Literal;

    fn rem(self, _rhs: f64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Decimal(n as f64 % _rhs),
            Literal::Decimal(n) => Literal::Decimal(n % _rhs),
            _ => todo!()
        }
    }
}

impl ops::Add<Literal> for Literal {
    type Output = Literal;

    fn add(self, _rhs: Literal) -> Literal {
        match _rhs {
            Literal::Integer(n) => self + n,
            Literal::Decimal(n) => self + n,
            Literal::String(n)  => self + n,
            _ => todo!()
        }
    }
}

impl ops::Sub<Literal> for Literal {
    type Output = Literal;

    fn sub(self, _rhs: Literal) -> Literal {
        match _rhs {
            Literal::Integer(n) => self - n,
            Literal::Decimal(n) => self - n,
            _ => todo!()
        }
    }
}


impl ops::Mul<Literal> for Literal {
    type Output = Literal;

    fn mul(self, _rhs: Literal) -> Literal {
        match _rhs {
            Literal::Integer(n) => self * n,
            Literal::Decimal(n) => self * n,
            Literal::String(n)  => self * n,
            _ => todo!()
        }
    }
}

impl ops::Div<Literal> for Literal {
    type Output = Literal;

    fn div(self, _rhs: Literal) -> Literal {
        match _rhs {
            Literal::Integer(n) => self / n,
            Literal::Decimal(n) => self / n,
            _ => todo!()
        }
    }
}

impl ops::Rem<Literal> for Literal {
    type Output = Literal;

    fn rem(self, _rhs: Literal) -> Literal {
        match _rhs {
            Literal::Integer(n) => self % n,
            Literal::Decimal(n) => self % n,
            _ => todo!()
        }
    }
}

pub trait Pow<T> {
    type Output;
    fn pow(self, rhs: T) -> Self::Output;
}

impl Pow<i64> for Literal {
    type Output = Literal;

    fn pow(self, _rhs: i64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(n.pow(_rhs.try_into().unwrap())),
            Literal::Decimal(n) => Literal::Decimal(n.powf(_rhs as f64)),
            _ => todo!()
        }
    }
}

impl Pow<f64> for Literal {
    type Output = Literal;

    fn pow(self, _rhs: f64) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(n.pow((_rhs as i64).try_into().unwrap())),
            Literal::Decimal(n) => Literal::Decimal(n.powf(_rhs)),
            _ => todo!()
        }
    }
}

impl Pow<Literal> for Literal {
    type Output = Literal;

    fn pow(self, _rhs: Literal) -> Literal {
        match _rhs {
            Literal::Integer(n) => self.pow(n),
            Literal::Decimal(n) => self.pow(n),
            _ => todo!()
        }
    }
}

pub trait Bang {
    type Output;
    fn bang(self) -> Self::Output;
}

impl Bang for Literal {
    type Output = Literal;

    fn bang(self) -> Literal {
        match self {
            Literal::Boolean(n) => Literal::Boolean(!n),
            _ => todo!()
        }
    }
}

pub trait Sub {
    type Output;
    fn sub(self) -> Self::Output;
}

impl Sub for Literal {
    type Output = Literal;

    fn sub(self) -> Literal {
        match self {
            Literal::Integer(n) => Literal::Integer(-n),
            Literal::Decimal(n) => Literal::Decimal(-n),
            _ => todo!()
        }
    }
}



// #[derive(Debug, PartialEq, Clone)]
// pub struct BinaryExpr {
//     pub op: BinaryOp,
//     pub left: Box<Node<Expr>>,
//     pub right: Box<Node<Expr>>
// }

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    LiteralExpr(Literal),
    ArrExpr(Vec<Node<Expr>>),
    IdentExpr(String),
    IndexExpr{ expr: Box<Node<Expr>>, index: Box<Node<Expr>> },
    BinaryExpr{ op: BinaryOp, left: Box<Node<Expr>>, right: Box<Node<Expr>> },
    UnaryExpr{ op: UnaryOp, expr: Box<Node<Expr>> },
    AssignExpr{ left: Box<Node<Expr>>, right: Box<Node<Expr>> },
    BlockExpr{ block: Vec<Node<Stmt>> },
    // TODO: String ?
    MethodCallExpr { receiver: String, method: String, args: Vec<Node<Expr>> },
    FunctionCallExpr { func: String, args: Vec<Node<Expr>> },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    LocalDeclaration(String, Node<Expr>),
    FunctionDeclaration(String, Vec<Node<Expr>>, Vec<Node<Stmt>>),
    Semi(Node<Expr>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Node<T> {
    pub val: T,
    pub span: Span
}

impl<T> Node<T> {
    pub fn new(val: T, span: Span) -> Self {
        Self {
            val,
            span
        }
    }
}