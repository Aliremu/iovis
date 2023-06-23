use crate::{lexer::{Lexer, Token, Literal as lexer_Literal, Span}, ast::{Expr, Literal, BinaryOp, Node, UnaryOp, Stmt}};
// use crate::ast::{Literal};

pub struct Parser {
    lexer: Lexer,
    tokens: Vec<(Token, Span)>
}


// AssignmentExpr
// MemberExpr
// FunctionCall
// LogicalExpr
// ComparisonExpr
// AdditiveExpr
// MultiplicativeExpr
// UnaryExpr
// PrimaryExpr

impl Parser {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        
        while let Ok(token) = lexer.next_token() {
            if token.0 == Token::EOF {
                break;
            }

            tokens.push(token);
        }

        // println!("{:?}", tokens);

        Self {
            lexer,
            tokens
        }
    }

    pub fn peek(&mut self) -> &(Token, Span) {
        self.tokens.get(0).unwrap()
    }

    pub fn chomp(&mut self) -> (Token, Span) {
        self.tokens.remove(0)
    }

    pub fn expect(&mut self, token: Token) -> (Token, Span) {
        if self.peek().0 == token {
            return self.chomp();
        } else {
            panic!("Expected {:?} at {:?}", token, self.peek().1);
        }
    }

    pub fn parse(&mut self) -> Vec<Node<Stmt>> {
        let mut body = Vec::new();

        while !self.tokens.is_empty() {
            body.push(self.parse_stmt());
            // println!("{:?}", self.tokens);
        }

        body
    }

    pub fn parse_stmt(&mut self) -> Node<Stmt> {
        match &self.peek().0 {
            Token::Let => self.parse_declaration(),
            Token::Fn => self.parse_function(),
            _ => {
                if !self.tokens.is_empty() {
                    let start = self.peek().1.start;
                    let expr = self.parse_expr();
                    let end = self.peek().1.end;

                    self.expect(Token::SemiColon);

                    return Node::new(Stmt::Semi(expr), Span { start, end });
                } else {
                    todo!();
                }
            }
        }
    }

    pub fn parse_declaration(&mut self) -> Node<Stmt> {
        if !self.tokens.is_empty() && self.peek().0 == Token::Let {
            let start = self.peek().1.start;

            self.chomp();

            if let Token::Ident(ident) = self.chomp().0 {
                self.expect(Token::Assign);

                let expr = self.parse_expr();

                let end = self.peek().1.end;

                self.expect(Token::SemiColon);
        
                return Node::new(Stmt::LocalDeclaration(ident, expr), Span { start, end });
            }
        }

        todo!("HUH");
    }

    pub fn parse_function(&mut self) -> Node<Stmt> {
        if !self.tokens.is_empty() && self.peek().0 == Token::Fn {
            let start = self.peek().1.start;

            self.chomp();

            if let Token::Ident(ident) = self.chomp().0 {
                self.expect(Token::LeftParen);

                let mut args = Vec::new();

                while !self.tokens.is_empty() && self.peek().0 != Token::RightParen {
                    let arg = self.parse_expr();
                    args.push(arg);
                    if self.peek().0 != Token::RightParen {
                        self.expect(Token::Comma);
                    }
                }

                self.chomp();

                self.expect(Token::LeftBrace);

                let mut block = Vec::new();

                while !self.tokens.is_empty() && self.peek().0 != Token::RightBrace {
                    let stmt = self.parse_stmt();
                    block.push(stmt);
                }

                let end = self.peek().1.end;

                self.chomp();
        
                return Node::new(Stmt::FunctionDeclaration(ident, args, block), Span { start, end });
            }
        }

        todo!("HUH");
    }

    pub fn parse_expr(&mut self) -> Node<Expr> {
        self.parse_assign()
    }

    pub fn parse_assign(&mut self) -> Node<Expr> {
        let mut left = self.parse_comparison();

        if !self.tokens.is_empty() && self.peek().0 == Token::Assign {
            self.chomp();

            let val = self.parse_assign();

            let span = Span { start: left.span.start, end: val.span.end };
        
            left = Node::new(Expr::AssignExpr { left: Box::new(left), right: Box::new(val) }, span);
        }

        left
    }

    pub fn parse_comparison(&mut self) -> Node<Expr> {
        let mut left = self.parse_add();

        while !self.tokens.is_empty() && (
            self.peek().0 == Token::Equal || 
            self.peek().0 == Token::NotEqual ||
            self.peek().0 == Token::Greater ||
            self.peek().0 == Token::GreaterEqual ||
            self.peek().0 == Token::Less ||
            self.peek().0 == Token::LessEqual
        ) {
            let op = match self.chomp().0 {
                Token::Equal        => BinaryOp::Eq,
                Token::NotEqual     => BinaryOp::Ne,
                Token::Greater      => BinaryOp::Gt,
                Token::GreaterEqual => BinaryOp::Ge,
                Token::Less         => BinaryOp::Lt,
                Token::LessEqual    => BinaryOp::Le,
                _ => {
                    break;
                }
            };

            let right = self.parse_add();

            let span = Span { start: left.span.start, end: right.span.end };

            left = Node::new(Expr::BinaryExpr {
                    op, 
                    left: Box::new(left), 
                    right: Box::new(right)
                }, span);
        }

        left
    }

    pub fn parse_add(&mut self) -> Node<Expr> {
        let mut left = self.parse_mul();

        while !self.tokens.is_empty() && (
            self.peek().0 == Token::Plus || 
            self.peek().0 == Token::Minus
        ) {
            let op = match self.chomp().0 {
                Token::Plus  => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => {
                    break;
                }
            };

            let right = self.parse_mul();

            let span = Span { start: left.span.start, end: right.span.end };

            left = Node::new(Expr::BinaryExpr {
                    op, 
                    left: Box::new(left), 
                    right: Box::new(right)
                }, span);
        }

        left
    }

    pub fn parse_mul(&mut self) -> Node<Expr> {
        let mut left = self.parse_exp();

        while !self.tokens.is_empty() && (
            self.peek().0 == Token::Asterisk || 
            self.peek().0 == Token::Slash || 
            self.peek().0 == Token::Modulo
        ) {
            let op = match self.chomp().0 {
                Token::Asterisk => BinaryOp::Mul,
                Token::Slash    => BinaryOp::Div,
                Token::Modulo   => BinaryOp::Mod,
                _ => {
                    break;
                }
            };

            let right = self.parse_exp();

            let span = Span { start: left.span.start, end: right.span.end };

            left = Node::new(Expr::BinaryExpr {
                op, 
                left: Box::new(left), 
                right: Box::new(right)
            }, span);
        }

        left
    }

    pub fn parse_exp(&mut self) -> Node<Expr> {
        let mut left = self.parse_unary();

        while !self.tokens.is_empty() && (
            self.peek().0 == Token::Carat
        ) {
            let op = match self.chomp().0 {
                Token::Carat => BinaryOp::Pow,
                _ => {
                    break;
                }
            };

            let right = self.parse_unary();

            let span = Span { start: left.span.start, end: right.span.end };

            left = Node::new(Expr::BinaryExpr {
                op, 
                left: Box::new(left), 
                right: Box::new(right)
            }, span);
        }

        left
    }

    pub fn parse_unary(&mut self) -> Node<Expr> {
        if !self.tokens.is_empty() && (
            self.peek().0 == Token::Bang || 
            self.peek().0 == Token::Minus
        ) {
            let op = match self.chomp().0 {
                Token::Bang  => UnaryOp::Bang,
                Token::Minus => UnaryOp::Sub,
                _ => todo!()
            };

            // TODO : parse_primary. does it break?
            let expr = self.parse_array();

            let span = Span { start: expr.span.start - 1, end: expr.span.end };

            Node::new(Expr::UnaryExpr {
                op, 
                expr: Box::new(expr)
            }, span)
        } else {
            self.parse_array() 
        }
    }

    pub fn parse_array(&mut self) -> Node<Expr> {
        while !self.tokens.is_empty() && (
            self.peek().0 == Token::LeftBracket
        ) {
            let start = self.peek().1.start;

            self.chomp();

            let mut args = Vec::new();

            while !self.tokens.is_empty() && self.peek().0 != Token::RightBracket {
                let arg = self.parse_expr();
                args.push(arg);
                if self.peek().0 != Token::RightBracket {
                    self.expect(Token::Comma);
                }
            }

            self.chomp();

            let end = self.peek().1.end;

            let span = Span { start, end };

            return Node::new(Expr::ArrExpr(args), span);
        }

        self.parse_call()
    }

    pub fn parse_call(&mut self) -> Node<Expr> {
        let mut object = self.parse_member();

        while !self.tokens.is_empty() && (
            self.peek().0 == Token::LeftParen
        ) {
            let start = self.peek().1.start;

            self.chomp();

            let mut args = Vec::new();

            while !self.tokens.is_empty() && self.peek().0 != Token::RightParen {
                let arg = self.parse_expr();
                args.push(arg);
                if self.peek().0 != Token::RightParen {
                    self.expect(Token::Comma);
                }
            }

            self.chomp();

            let end = self.peek().1.end;

            let span = Span { start, end };

            if let Expr::IdentExpr(func) = object.val {
                object = Node::new(Expr::FunctionCallExpr { func, args }, span);
            }
        }

        object
    }

    pub fn parse_member(&mut self) -> Node<Expr> {
        let mut object = self.parse_index();

        while !self.tokens.is_empty() && (
            self.peek().0 == Token::Dot
        ) {
            let start = self.peek().1.start;

            self.chomp();

            let member = self.parse_primary();

            if self.peek().0 == Token::LeftParen {
                self.chomp();

                let mut args = Vec::new();

                while !self.tokens.is_empty() && self.peek().0 != Token::RightParen {
                    let arg = self.parse_expr();
                    args.push(arg);
                    if self.peek().0 != Token::RightParen {
                        self.expect(Token::Comma);
                    }
                }
    
                self.chomp();

                let end = self.peek().1.end;

                let span = Span { start, end };

                if let (Expr::IdentExpr(receiver), Expr::IdentExpr(method)) = (object.val.clone(), member.val) {
                    return Node::new(Expr::MethodCallExpr { receiver, method, args }, span);
                }
            } else {
                return object;
            }
        }

        object
    }

    pub fn parse_index(&mut self) -> Node<Expr> {
        let mut object = self.parse_primary();

        while !self.tokens.is_empty() && (
            self.peek().0 == Token::LeftBracket
        ) {
            let start = self.peek().1.start;

            self.chomp();

            // let mut args = Vec::new();

            // while !self.tokens.is_empty() && self.peek().0 != Token::RightParen {
            //     let arg = self.parse_expr();
            //     args.push(arg);
            //     if self.peek().0 != Token::RightParen {
            //         self.expect(Token::Comma);
            //     }
            // }

            let index = self.parse_expr();
            
            self.chomp();

            let end = self.peek().1.end;

            let span = Span { start, end };

            object = Node::new(Expr::IndexExpr { expr: Box::new(object), index: Box::new(index) }, span);
        }

        object
    }

    pub fn parse_primary(&mut self) -> Node<Expr> {
        let expr = match &self.peek().0 {
            Token::Ident(ident) => Expr::IdentExpr(ident.to_owned()),
            Token::Literal(lit) => {
                match lit {
                    lexer_Literal::Boolean(n) => Expr::LiteralExpr(Literal::Boolean(n.to_owned())),
                    lexer_Literal::Integer(n) => Expr::LiteralExpr(Literal::Integer(n.to_owned())),
                    lexer_Literal::Decimal(n) => Expr::LiteralExpr(Literal::Decimal(n.to_owned())),
                    lexer_Literal::String(n)  => Expr::LiteralExpr(Literal::String(n.to_owned()))
                }
            },
            Token::LeftParen => {
                self.chomp();
                let node = self.parse_expr();
                if self.tokens.is_empty() || self.peek().0 != Token::RightParen {
                    todo!("Unbalanced parentheses at {:?}", node.span);
                }

                node.val
            }

            _ => todo!("{:?}", self.peek())
        };

        let chomp = self.chomp();

        Node::new(expr, chomp.1)
    }
}