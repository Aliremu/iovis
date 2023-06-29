use crate::{
    ast::{Bang, BinaryExpr, BinaryOp, Expr, Literal, Node, Pow, Stmt, Sub, UnaryExpr, UnaryOp, IndexExpr, ArrExpr, MethodCallExpr, FunctionCallExpr, AssignExpr, LocalDeclaration, FunctionDeclaration},
    lexer::Span,
};
use std::collections::HashMap;
use std::fmt::Write;

fn evaluate_binary_expr(ctx: &mut Context, expr: &BinaryExpr) -> Expr {
    let left = evaluate(ctx, *expr.clone().left);
    let right = evaluate(ctx, *expr.clone().right);

    if let (Expr::LiteralExpr(left_lit), Expr::LiteralExpr(right_lit)) = (left, right) {
        let out = match expr.op {
            BinaryOp::Add => left_lit + right_lit,
            BinaryOp::Sub => left_lit - right_lit,
            BinaryOp::Mul => left_lit * right_lit,
            BinaryOp::Div => left_lit / right_lit,
            BinaryOp::Mod => left_lit % right_lit,
            BinaryOp::Pow => left_lit.pow(right_lit),
            BinaryOp::Eq => Literal::Boolean(left_lit.eq(&right_lit)),
            BinaryOp::Ne => Literal::Boolean(left_lit.ne(&right_lit)),
            BinaryOp::Gt => Literal::Boolean(left_lit.gt(&right_lit)),
            BinaryOp::Ge => Literal::Boolean(left_lit.ge(&right_lit)),
            BinaryOp::Lt => Literal::Boolean(left_lit.lt(&right_lit)),
            BinaryOp::Le => Literal::Boolean(left_lit.le(&right_lit)),
            _ => todo!(),
        };

        return Expr::LiteralExpr(out);
    }

    panic!("wtf");
}

fn evaluate_unary_expr(ctx: &mut Context, expr: &UnaryExpr) -> Expr {
    let right = evaluate(ctx, *expr.clone().expr);

    if let Expr::LiteralExpr(lit) = right {
        let out = match expr.op {
            UnaryOp::Sub => lit.sub(),
            UnaryOp::Bang => lit.bang(),
            _ => todo!(),
        };

        return Expr::LiteralExpr(out);
    }

    todo!();
}

fn evaluate_ident(ctx: &Context, node: Expr) -> Expr {
    if let Expr::IdentExpr(ident) = &node {
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

fn evaluate_assign(ctx: &mut Context, node: Expr) -> Expr {
    if let Expr::AssignExpr(AssignExpr { left, right }) = node {
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

fn run_function(ctx: &mut Context, func: &String, args: Vec<Node<Expr>>) -> Expr {
    let func = ctx.functions.get(func).unwrap();

    if let RuntimeVal::NativeFunction(func) = &func {
        return func(ctx, args);
    } else if let RuntimeVal::Function(block) = &func {
        for stmt in block.clone() {
            ctx.evaluate_stmt(stmt);
        }

        return Expr::LiteralExpr(Literal::Boolean(true));
    }

    panic!("Undeclared function");
}

fn evaluate_call(ctx: &mut Context, node: Node<Expr>) -> Expr {
    if let Expr::FunctionCallExpr(FunctionCallExpr { func, args }) = &node.val {
        let mut vals = Vec::new();

        for arg in args {
            vals.push(Node::new(evaluate(ctx, arg.clone()), Span { start: 0, end: 0}));
        }

        if ctx.functions.contains_key(func) {
            return run_function(ctx, func, vals);
        } else {
            if ctx.variables.contains_key(func) {
                if let Expr::IdentExpr(eval) = ctx.get_var(func) {
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

fn evaluate_arr(ctx: &mut Context, node: Node<Expr>) -> Expr {
    if let Expr::ArrExpr(ArrExpr { elems }) = &node.val {
        let mut vals = Vec::new();

        for elem in elems {
            vals.push(Node::new(evaluate(ctx, elem.clone()), Span { start: 0, end: 0 }));
        }

        return Expr::ArrExpr(ArrExpr { elems: vals });
    }

    todo!();
}

fn evaluate(ctx: &mut Context, node: Node<Expr>) -> Expr {
    match &node.val {
        Expr::BinaryExpr(expr) => evaluate_binary_expr(ctx, &expr),

        Expr::UnaryExpr(expr) => evaluate_unary_expr(ctx, &expr),

        Expr::IdentExpr { .. } => evaluate_ident(ctx, node.val),

        Expr::AssignExpr { .. } => evaluate_assign(ctx, node.val),

        Expr::FunctionCallExpr { .. } => {
            let copy = ctx.variables.clone();

            let out = evaluate_call(ctx, node);

            ctx.variables = ctx
                .variables
                .clone()
                .into_iter()
                .filter(|x| copy.contains_key(&x.0))
                .collect::<HashMap<String, Expr>>();

            out
        }

        Expr::LiteralExpr(lit) => node.val,

        Expr::ArrExpr { .. } => evaluate_arr(ctx, node),

        Expr::IndexExpr(IndexExpr { expr, index }) => {
            if let Expr::ArrExpr(ArrExpr { elems }) = evaluate(ctx, *expr.clone()) {
                let idx = evaluate(ctx, *index.clone());

                if let Expr::LiteralExpr(Literal::Integer(n)) = idx {
                    return evaluate(ctx, elems[n as usize].clone());
                }

                panic!("Index must be an integer!");
            }

            panic!("This type is not indexable!");
        }

        Expr::MethodCallExpr(MethodCallExpr {
            receiver,
            method,
            args,
        }) => {
            if method == "det" {
                if let Expr::ArrExpr(ArrExpr { elems }) = ctx.get_var(receiver) {
                    let arr0 = &elems[0].val;
                    let arr1 = &elems[1].val;

                    if let (Expr::ArrExpr(ArrExpr { elems: arr3 }), Expr::ArrExpr(ArrExpr { elems: arr4 })) =
                        (arr0, arr1)
                    {
                        if let (
                            Expr::LiteralExpr(a),
                            Expr::LiteralExpr(b),
                            Expr::LiteralExpr(c),
                            Expr::LiteralExpr(d),
                        ) = (
                            arr3[0].clone().val,
                            arr3[1].clone().val,
                            arr4[0].clone().val,
                            arr4[1].clone().val,
                        ) {
                            return Expr::LiteralExpr(Literal::from(a * d - b * c));
                        }
                    }
                }
            }

            panic!("not");
        }

        _ => panic!(),
    }
}

#[derive(Clone)]
pub struct Context {
    variables: HashMap<String, Expr>,
    functions: HashMap<String, RuntimeVal>,
    pub output: String,
}

#[derive(Clone)]
enum RuntimeVal {
    NativeFunction(fn(&mut Context, Vec<Node<Expr>>) -> Expr),
    Function(Vec<Node<Stmt>>),
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Context {
            variables: HashMap::new(),
            functions: HashMap::new(),
            output: String::new(),
        };

        // ctx.declare_var("print", )
        let a = |context: &mut Context, args: Vec<Node<Expr>>| {
            let m = args
                .iter()
                .filter_map(|x| match x.val.clone() {
                    Expr::LiteralExpr(lit) => Some(lit.to_string()),
                    _ => None,
                })
                .collect::<Vec<String>>()
                .join(",");

            // println!("{}", m);
            writeln!(context.output, "{}", m).unwrap();

            Expr::LiteralExpr(Literal::String(m))
        };

        ctx.declare_native_var("print".to_string(), a);

        ctx
    }

    pub fn flush(&mut self) -> String {
        let ret = self.output.clone();
        self.output.clear();

        ret
    }

    pub fn declare_var(&mut self, ident: String, node: Expr) {
        self.variables.insert(ident, node);
    }

    pub fn declare_native_var(
        &mut self,
        ident: String,
        func: fn(&mut Context, Vec<Node<Expr>>) -> Expr,
    ) {
        self.functions
            .insert(ident, RuntimeVal::NativeFunction(func));
    }

    pub fn get_var(&self, ident: &String) -> Expr {
        self.variables
            .get(ident)
            .expect("This variable has not been declared!")
            .clone()
    }

    pub fn evaluate_stmt(&mut self, node: Node<Stmt>) {
        match &node.val {
            Stmt::LocalDeclaration(LocalDeclaration { ident, value }) => {
                let val = evaluate(self, value.clone());
                self.variables.insert(ident.val.clone(), val);
            }

            Stmt::FunctionDeclaration(FunctionDeclaration { ident, params, output, block }) => {
                self.functions
                    .insert(ident.val.clone(), RuntimeVal::Function(block.val.stmts.clone()));
            }

            Stmt::Semi(expr) => {
                evaluate(self, expr.clone());
            }

            _ => panic!(),
        }
    }
}
