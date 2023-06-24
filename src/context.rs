
use std::collections::HashMap;
use std::fmt::Write;
use crate::{parser::Parser, ast::{Expr, Node, BinaryOp, Pow, Bang, Sub, UnaryOp, Literal, Stmt}, lexer::Span};

fn evaluate_binary_expr(ctx: &mut Context, node: Node<Expr>) -> Node<Expr> {
    if let Expr::BinaryExpr { op, left, right } = &node.val {
        let left  = evaluate(ctx, *left.clone());
        let right = evaluate(ctx, *right.clone());

        if let (Expr::LiteralExpr(left_lit), Expr::LiteralExpr(right_lit)) = (left.val, right.val) {
            let out = match op {
                BinaryOp::Add => left_lit + right_lit,
                BinaryOp::Sub => left_lit - right_lit,
                BinaryOp::Mul => left_lit * right_lit,
                BinaryOp::Div => left_lit / right_lit,
                BinaryOp::Mod => left_lit % right_lit,
                BinaryOp::Pow => left_lit.pow(right_lit),
                BinaryOp::Eq  => Literal::Boolean(left_lit.eq(&right_lit)),
                BinaryOp::Ne  => Literal::Boolean(left_lit.ne(&right_lit)),
                BinaryOp::Gt  => Literal::Boolean(left_lit.gt(&right_lit)),
                BinaryOp::Ge  => Literal::Boolean(left_lit.ge(&right_lit)),
                BinaryOp::Lt  => Literal::Boolean(left_lit.lt(&right_lit)),
                BinaryOp::Le  => Literal::Boolean(left_lit.le(&right_lit)),
                _ => todo!()
            };

            return Node::new(Expr::LiteralExpr(out), node.span);
        }
    }

    todo!("{:?}", node);
}

fn evaluate_unary_expr(ctx: &mut Context, node: Node<Expr>) -> Node<Expr> {
    if let Expr::UnaryExpr { op, expr } = node.val {
        let right = evaluate(ctx, *expr);

        if let Expr::LiteralExpr(lit) = right.val {
            let out = match op {
                UnaryOp::Sub  => lit.sub(),
                UnaryOp::Bang => lit.bang(),
                _ => todo!()
            };

            return Node::new(Expr::LiteralExpr(out), node.span);
        }
    }

    todo!();
}

fn evaluate_ident(ctx: &Context, node: Node<Expr>) -> Node<Expr> {
    if let Expr::IdentExpr(ident) = &node.val {
        if !ctx.variables.contains_key(ident) {
            if ctx.functions.contains_key(ident) {
                return node;
            }
        } else {
            return ctx.get_var(&ident);
        }
    }

    todo!();
}

fn evaluate_assign(ctx: &mut Context, node: Node<Expr>) -> Node<Expr> {
    if let Expr::AssignExpr { left, right } = node.val {
        if let Expr::IdentExpr(ident) = left.val {
            if ctx.variables.contains_key(&ident) {
                let val = evaluate(ctx, *right);
                ctx.declare_var(ident, val.clone());
                return val;
            } else {
                panic!("Can not assign a value to an undeclared variable!");
            }
        }
    }

    todo!();
}

fn run_function(ctx: &mut Context, func: &String, args: Vec<Node<Expr>>) -> Node<Expr> {
    let func = ctx.functions.get(func).unwrap();

    if let RuntimeVal::NativeFunction(func) = &func {
        return func(ctx, args);
    } else if let RuntimeVal::Function(block) = &func {
        for stmt in block.clone() {
            ctx.evaluate_stmt(stmt);
        }

        return Node::new(Expr::LiteralExpr(Literal::Boolean(true)), Span { start: 0, end: 0 });
    }

    panic!("Undeclared function");
}

fn evaluate_call(ctx: &mut Context, node: Node<Expr>) -> Node<Expr> {
    if let Expr::FunctionCallExpr { func, args } = &node.val {
        let mut vals = Vec::new();
        
        for arg in args {
            vals.push(evaluate(ctx, arg.clone()));
        }

        if ctx.functions.contains_key(func) {
            return run_function(ctx, func, vals);
        } else {
            if ctx.variables.contains_key(func) {
                if let Expr::IdentExpr(eval) = ctx.get_var(func).val {
                    if ctx.functions.contains_key(&eval) {
                        return run_function(ctx, &eval, vals);
                    }
                }
            }
        }

        // if func == "print" {
        //     println!("{:?}", vals);
        //     return node;
        // } else {
        //     panic!("TODO");
        // }
    }

    todo!();
}

fn evaluate(ctx: &mut Context, node: Node<Expr>) -> Node<Expr> {
    match &node.val {
        Expr::BinaryExpr { .. } => {
            evaluate_binary_expr(ctx, node)
        },

        Expr::UnaryExpr { .. } => {
            evaluate_unary_expr(ctx, node)
        },

        Expr::IdentExpr { .. } => {
            evaluate_ident(ctx, node)
        },

        Expr::AssignExpr { .. } => {
            evaluate_assign(ctx, node)
        },

        Expr::FunctionCallExpr { .. } => {
            evaluate_call(ctx, node)
        }

        Expr::LiteralExpr(_) => {
            node
        },

        Expr::ArrExpr(..) => {
            node
        },

        Expr::IndexExpr { expr, index } => {
            if let Expr::ArrExpr(arr) = evaluate(ctx, *expr.clone()).val {
                let idx = evaluate(ctx, *index.clone());

                if let Expr::LiteralExpr(Literal::Integer(n)) = idx.val {
                    return evaluate(ctx, arr[n as usize].clone());
                }

                panic!("Index must be an integer!");
            }

            panic!("This type is not indexable!");
        },

        Expr::MethodCallExpr { receiver, method, args } => {
            if method == "det" {
                if let Expr::ArrExpr(arr) = ctx.get_var(receiver).val {
                    let arr0 = &arr[0].val;
                    let arr1 = &arr[1].val;

                    if let (Expr::ArrExpr(arr3), Expr::ArrExpr(arr4)) = (arr0, arr1) {
                        if let (Expr::LiteralExpr(a), Expr::LiteralExpr(b), Expr::LiteralExpr(c), Expr::LiteralExpr(d)) = (evaluate(ctx, arr3[0].clone()).val, evaluate(ctx, arr3[1].clone()).val, evaluate(ctx, arr4[0].clone()).val, evaluate(ctx, arr4[1].clone()).val) {
                            return Node::new(Expr::LiteralExpr(Literal::from(a * d - b * c)), Span { start: 0, end: 0 });
                        }
                    }
                }
            }

            panic!("not");
        }

        _ => panic!()
    }
}

#[derive(Clone)]
pub struct Context {
    variables: HashMap<String, Node<Expr>>,
    functions: HashMap<String, RuntimeVal>,
    pub output: String
}

#[derive(Clone)]
enum RuntimeVal {
    NativeFunction(fn(&mut Context, Vec<Node<Expr>>) -> Node<Expr>),
    Function(Vec<Node<Stmt>>)
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Context {
            variables: HashMap::new(),
            functions: HashMap::new(),
            output: String::new()
        };

        // ctx.declare_var("print", )
        let a = |context: &mut Context, args: Vec<Node<Expr>>| {
            let m = args.iter().filter_map(|x| match x.val.clone() {
                Expr::LiteralExpr(lit) => Some(lit.to_string()),
                _ => None,
            }).collect::<Vec<String>>().join(",");

            // println!("{}", m);
            writeln!(context.output, "{}", m);

            Node::new(Expr::LiteralExpr(Literal::String(m)), Span { start: 0, end: 0 })
        };

        ctx.declare_native_var("print".to_string(), a);

        ctx
    }

    pub fn flush(&mut self) -> String {
        let ret = self.output.clone();
        self.output.clear();

        ret
    }

    pub fn declare_var(&mut self, ident: String, node: Node<Expr>) {
        self.variables.insert(ident, node);
    }

    pub fn declare_native_var(&mut self, ident: String, func: fn(&mut Context, Vec<Node<Expr>>) -> Node<Expr>) {
        self.functions.insert(ident, RuntimeVal::NativeFunction(func));
    }

    pub fn get_var(&self, ident: &String) -> Node<Expr> {
        self.variables.get(ident).expect("This variable has not been declared!").clone()
    }

    pub fn evaluate_stmt(&mut self, node: Node<Stmt>) {
        match &node.val {
            Stmt::LocalDeclaration(ident, expr) => {
                let val = evaluate(self, expr.clone());
                self.variables.insert(ident.to_owned(), val);
            },
    
            Stmt::FunctionDeclaration(func, args, block) => {
                self.functions.insert(func.to_string(), RuntimeVal::Function(block.clone()));
            },
    
            Stmt::Semi(expr) => {
                evaluate(self, expr.clone());
            },
    
            _ => panic!()
        }
    }
}