use std::collections::VecDeque;

use crate::{
    ast::{
        ArrExpr, AssignExpr, BinaryExpr, BinaryOp, Block, Expr, FunctionCallExpr,
        FunctionDeclaration, IndexExpr, LocalDeclaration, MethodCallExpr, Parameter,
        Return, Stmt, UnaryExpr, UnaryOp, ExprKind, StmtKind, NamedParameter, If, While, Field, Struct, MemberExpr, TyKind,
    },
    err::CompileError,
    lexer::{Lexer, Span, TokenKind, Token},
};

pub struct Parser {
    lexer: Lexer,
    tokens: VecDeque<Token>,
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
        let mut tokens = VecDeque::new();

        while let Ok(token) = lexer.next_token() {
            if token.kind == TokenKind::EOF {
                break;
            }

            tokens.push_back(token);
        }

        // println!("{:#?}", tokens);

        Self { lexer, tokens }
    }

    pub fn peek(&mut self) -> Result<&Token, CompileError> {
        match self.tokens.get(0) {
            Some(val) => Ok(val),
            None => Err(CompileError::Other(
                "Reached EOF without expected token".to_string(),
            )),
        }
    }

    pub fn chomp(&mut self) -> Result<Token, CompileError> {
        match self.tokens.pop_front() {
            Some(val) => Ok(val),
            None => Err(CompileError::Other(
                "Reached EOF without expected token".to_string(),
            )),
        }
    }

    pub fn expect(&mut self, token: TokenKind) -> Result<Token, CompileError> {
        let peek = self.peek()?.clone();
        if peek.kind == token {
            self.chomp()
        } else {
            Err(CompileError::UnexpectedToken(peek.kind, token, peek.span.start))
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, CompileError> {
        let mut body = Vec::new();

        while !self.tokens.is_empty() {
            body.push(self.parse_stmt()?);
        }

        Ok(body)
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, CompileError> {
        match &self.peek()?.kind {
            TokenKind::Let    => self.parse_declaration(),
            TokenKind::Fn     => self.parse_function(),
            TokenKind::Return => self.parse_return(),
            TokenKind::If     => self.parse_if(),
            TokenKind::While  => self.parse_while(),
            TokenKind::Import => self.parse_import(),
            TokenKind::Extern => self.parse_extern(),
            TokenKind::Struct => self.parse_struct(),
            _                 => self.parse_semi() 
        }
    }

    fn parse_import(&mut self) -> Result<Stmt, CompileError> {
        let start = self.chomp()?.span;
        let ident = self.parse_primary()?.into();

        let end = self.expect(TokenKind::SemiColon)?.span;
        let span = Span::merge(start, end);
        let kind = StmtKind::Import(ident);

        Ok(Stmt { 
            kind, 
            span
        })
    }

    fn parse_struct(&mut self) -> Result<Stmt, CompileError> {
        let start = self.chomp()?.span;

        let ident = self.parse_primary()?.into();

        self.expect(TokenKind::LeftBrace)?;

        let mut fields = Vec::new();

        while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightBrace {
            let ident = self.parse_primary()?.into();
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_primary()?.into();

            let field = Field {
                ident,
                ty
            };

            fields.push(field);

            if self.peek()?.kind != TokenKind::RightBrace {
                self.expect(TokenKind::Comma)?;
            }
        }

        let end = self.expect(TokenKind::RightBrace)?.span;
        let span = Span::merge(start, end);
        let kind = StmtKind::Struct(Struct {
            ident,
            fields
        });

        Ok(Stmt {
            kind,
            span
        })
    }

    fn parse_semi(&mut self) -> Result<Stmt, CompileError> {
        let expr = self.parse_expr()?;

        let start = expr.span;
        let end = self.expect(TokenKind::SemiColon)?.span;

        while !self.tokens.is_empty() && self.peek()?.kind == TokenKind::SemiColon {
            self.chomp()?;
        }

        let span = Span::merge(start, end);
        let kind = StmtKind::Semi(expr);

        Ok(Stmt { 
            kind, 
            span
        })
    }

    fn parse_return(&mut self) -> Result<Stmt, CompileError> {
        let start = self.chomp()?.span;

        let mut ret = None;

        if self.peek()?.kind != TokenKind::SemiColon {
            ret.insert(self.parse_expr()?);
        }

        let end = self.expect(TokenKind::SemiColon)?.span;

        let span = Span::merge(start, end);
        let kind = StmtKind::Return(Return { ret });
        Ok(Stmt {
            kind, 
            span
        })
    }

    fn parse_if(&mut self) -> Result<Stmt, CompileError> {
        let start = self.chomp()?.span;

        let cond = self.parse_expr()?;

        self.expect(TokenKind::LeftBrace)?;

        let mut stmts = Vec::new();

        while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightBrace {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        let then_branch = Block { stmts };
        let mut else_branch = None;

        let mut end = self.chomp()?.span;

        if !self.tokens.is_empty() && self.peek()?.kind == TokenKind::Else {
            self.chomp()?;
            self.expect(TokenKind::LeftBrace)?;

            let mut stmts = Vec::new();

            while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightBrace {
                let stmt = self.parse_stmt()?;
                stmts.push(stmt);
            }

            else_branch.insert(Block { stmts });

            end = self.chomp()?.span;
        }

        let span = Span::merge(start, end);
        let kind = StmtKind::If(If { cond, then_branch, else_branch });
        Ok(Stmt {
            kind, 
            span
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, CompileError> {
        let start = self.chomp()?.span;

        let cond = self.parse_expr()?;
        
        self.expect(TokenKind::LeftBrace)?;
        let mut stmts = Vec::new();

        while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightBrace {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
        }

        let end = self.chomp()?.span;

        let span = Span::merge(start, end);
        let block = Block { stmts };
        let kind = StmtKind::While(While { cond, block });

        Ok(Stmt {
            kind, 
            span
        })
    }

    fn parse_extern(&mut self) -> Result<Stmt, CompileError> {
        let start = self.chomp()?.span;
        self.expect(TokenKind::LeftBrace)?;

        let mut stmts = Vec::new();

        while !self.tokens.is_empty() && self.peek()?.kind == TokenKind::Fn {
            let start = self.peek()?.span;

            self.chomp()?;

            if let TokenKind::Ident(ident) = self.chomp()?.kind {
                self.expect(TokenKind::LeftParen)?;

                let mut params = Vec::new();

                while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightParen {
                    if self.peek()?.kind == TokenKind::DotDot {
                        self.chomp()?;
                        params.push(Parameter::Variadic);
                    } else {
                        let ident = self.parse_primary()?.into();
                        self.expect(TokenKind::Colon)?;
                        let ty = self.parse_ty()?;
    
                        params.push(Parameter::NamedParameter(NamedParameter {
                            ident,
                            ty
                        }));
                    }

                    if self.peek()?.kind != TokenKind::RightParen {
                        self.expect(TokenKind::Comma)?;
                    }
                }

                self.chomp()?;

                let output = {
                    if self.peek()?.kind == TokenKind::Colon {
                        self.chomp()?;

                        self.parse_ty()?
                    } else {
                        TyKind::Void
                    }
                };

                let end = self.expect(TokenKind::SemiColon)?.span;

                let span = Span::merge(start, end);
                let kind = StmtKind::ForeignFn(ident, params, output);

                let stmt = Stmt {
                    kind,
                    span,
                };

                stmts.push(stmt);
            }
        }

        let end = self.expect(TokenKind::RightBrace)?.span;
        let span = Span::merge(start, end);
        let kind = StmtKind::Extern(Block { stmts });
        Ok(Stmt {
            kind,
            span,
        })
    }

    fn parse_declaration(&mut self) -> Result<Stmt, CompileError> {
        if !self.tokens.is_empty() && self.peek()?.kind == TokenKind::Let {
            let start = self.chomp()?.span;

            if let TokenKind::Ident(ident) = self.chomp()?.kind {

                let mut ty = None;

                if self.peek()?.kind == TokenKind::Colon {
                    self.chomp()?;

                    ty.insert(self.parse_ty()?);
                }

                let mut value = None; 
                
                if self.peek()?.kind == TokenKind::Eq {
                    self.chomp()?;

                    value.insert(self.parse_expr()?);
                }

                let end = self.expect(TokenKind::SemiColon)?.span;

                while !self.tokens.is_empty() && self.peek()?.kind == TokenKind::SemiColon {
                    self.chomp()?;
                }

                let span = Span::merge(start, end);
                let kind = StmtKind::LocalDeclaration(LocalDeclaration { 
                    ident, 
                    ty,
                    value 
                });

                return Ok(Stmt {
                    kind,
                    span,
                });
            }
        }

        Err(CompileError::Other(
            "Invalid variable declaration".to_string(),
        ))
    }

    fn parse_function(&mut self) -> Result<Stmt, CompileError> {
        if !self.tokens.is_empty() && self.peek()?.kind == TokenKind::Fn {
            let start = self.chomp()?.span;

            if let TokenKind::Ident(ident) = self.chomp()?.kind {
                self.expect(TokenKind::LeftParen)?;

                let mut params = Vec::new();

                while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightParen {
                    if self.peek()?.kind == TokenKind::DotDot {
                        self.chomp()?;
                        params.push(Parameter::Variadic);
                    } else {
                        let ident = self.parse_primary()?.into();
                        self.expect(TokenKind::Colon)?;
                        let ty = self.parse_ty()?;
                        

                        params.push(Parameter::NamedParameter(NamedParameter {
                            ident,
                            ty
                        }));
                    }

                    if self.peek()?.kind != TokenKind::RightParen {
                        self.expect(TokenKind::Comma)?;
                    }
                }

                self.chomp()?;

                let output = {
                    if self.peek()?.kind == TokenKind::Colon {
                        self.chomp()?;

                        self.parse_ty()?
                    } else {
                        TyKind::Void
                    }
                };

                self.expect(TokenKind::LeftBrace)?;

                let mut stmts = Vec::new();

                while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightBrace {
                    let stmt = self.parse_stmt()?;
                    stmts.push(stmt);
                }

                let end = self.chomp()?.span;

                let block = Block { stmts };
                let span = Span::merge(start, end);
                let kind = StmtKind::FunctionDeclaration(FunctionDeclaration {
                    ident,
                    params,
                    output,
                    block,
                });

                return Ok(Stmt {
                    kind,
                    span
                });
            }
        }

        Err(CompileError::Other(
            "Invalid function declaration".to_string(),
        ))
    }

    fn parse_ty(&mut self) -> Result<TyKind, CompileError> {
        let reference = self.peek()?.kind == TokenKind::And;
        if reference {
            self.chomp()?;
        }

        let ty_expr = self.parse_primary()?;

        let ty = if self.peek()?.kind == TokenKind::LeftBracket {
            self.chomp()?;
            self.expect(TokenKind::RightBracket)?;

            TyKind::Array(ty_expr.into())
        } else {
            TyKind::Reg(ty_expr.into())
        };

        if reference {
            Ok(TyKind::Reference(Box::new(ty)))
        } else {
            Ok(ty)
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, CompileError> {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_comparison()?;
        let start = left.span;

        if !self.tokens.is_empty() && self.peek()?.kind == TokenKind::Eq {
            self.chomp()?;

            let val = self.parse_assign()?;

            let end = val.span;

            let span = Span::merge(start, end);
            let kind = ExprKind::AssignExpr(AssignExpr {
                left: Box::new(left),
                right: Box::new(val),
            });

            left = Expr {
                kind,
                span,
            };
        }

        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_add()?;
        let start = left.span;

        while !self.tokens.is_empty()
            && (self.peek()?.kind == TokenKind::EqEq
                || self.peek()?.kind == TokenKind::NotEq
                || self.peek()?.kind == TokenKind::Gt
                || self.peek()?.kind == TokenKind::Ge
                || self.peek()?.kind == TokenKind::Lt
                || self.peek()?.kind == TokenKind::Le)
        {
            let op = match self.chomp()?.kind {
                TokenKind::EqEq => BinaryOp::Eq,
                TokenKind::NotEq => BinaryOp::Ne,
                TokenKind::Gt => BinaryOp::Gt,
                TokenKind::Ge => BinaryOp::Ge,
                TokenKind::Lt => BinaryOp::Lt,
                TokenKind::Le => BinaryOp::Le,
                _ => {
                    break;
                }
            };

            let right = self.parse_add()?;
            let end = right.span;

            let span = Span::merge(start, end);
            let kind = ExprKind::BinaryExpr(BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            });

            left = Expr {
                kind,
                span,
            };
        }

        Ok(left)
    }

    fn parse_add(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_mul()?;
        let start = left.span;

        while !self.tokens.is_empty()
            && (self.peek()?.kind == TokenKind::Plus || 
                self.peek()?.kind == TokenKind::Minus)
        {
            let op = match self.chomp()?.kind {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => {
                    break;
                }
            };

            let right = self.parse_mul()?;
            let end = right.span;

            let span = Span::merge(start, end);
            let kind = ExprKind::BinaryExpr(BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            });

            left = Expr {
                kind,
                span,
            };
        }

        Ok(left)
    }

    fn parse_mul(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_exp()?;
        let start = left.span;

        while !self.tokens.is_empty()
            && (self.peek()?.kind == TokenKind::Star
                || self.peek()?.kind == TokenKind::Slash
                || self.peek()?.kind == TokenKind::Percent)
        {
            let op = match self.chomp()?.kind {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => {
                    break;
                }
            };

            let right = self.parse_exp()?;
            let end = right.span;

            let span = Span::merge(start, end);
            let kind = ExprKind::BinaryExpr(BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            });

            left = Expr {
                kind,
                span,
            };
        }

        Ok(left)
    }

    fn parse_exp(&mut self) -> Result<Expr, CompileError> {
        let mut left = self.parse_unary()?;
        let start = left.span;

        while !self.tokens.is_empty() && (self.peek()?.kind == TokenKind::Caret) {
            let op = match self.chomp()?.kind {
                TokenKind::Caret => BinaryOp::Pow,
                _ => {
                    break;
                }
            };

            let right = self.parse_unary()?;
            let end = right.span;

            let span = Span::merge(start, end);
            let kind = ExprKind::BinaryExpr(BinaryExpr {
                op,
                left: Box::new(left),
                right: Box::new(right),
            });

            left = Expr {
                kind,   
                span,
            };
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Expr, CompileError> {
        if !self.tokens.is_empty() && (self.peek()?.kind == TokenKind::Not || self.peek()?.kind == TokenKind::Minus) {
            let start = self.peek()?.span;

            let op = match self.chomp()?.kind {
                TokenKind::Not => UnaryOp::Not,
                TokenKind::Minus => UnaryOp::Neg,
                _ => todo!(),
            };

            // TODO : parse_primary. does it break?
            let expr = self.parse_call()?;

            let span = Span::merge(start, expr.span);
            let kind = ExprKind::UnaryExpr(UnaryExpr {
                op,
                expr: Box::new(expr),
            });

            Ok(Expr {
                kind,    
                span,
            })
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Result<Expr, CompileError> {
        let mut object = self.parse_member()?;

        while !self.tokens.is_empty() && (self.peek()?.kind == TokenKind::LeftParen) {
            let start = self.chomp()?.span;

            let mut args = Vec::new();

            while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightParen {
                let arg = self.parse_expr()?;
                args.push(arg);
                if self.peek()?.kind != TokenKind::RightParen {
                    self.expect(TokenKind::Comma)?;
                }
            }

            let end = self.chomp()?.span;

            let span = Span::merge(start, end);

            if let ExprKind::IdentExpr(func) = object.kind {
                let kind = ExprKind::FunctionCallExpr(FunctionCallExpr { 
                    func, 
                    args 
                });

                object = Expr {
                    kind,
                    span,
                };
            }
        }

        Ok(object)
    }

    fn parse_member(&mut self) -> Result<Expr, CompileError> {
        let object = self.parse_index()?;

        while !self.tokens.is_empty() && (self.peek()?.kind == TokenKind::Dot) {
            let start = self.chomp()?.span;

            let member = self.parse_primary()?;

            if self.peek()?.kind == TokenKind::LeftParen {
                self.chomp()?;

                let mut args = Vec::new();

                while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightParen {
                    let arg = self.parse_expr()?;
                    args.push(arg);
                    if self.peek()?.kind != TokenKind::RightParen {
                        self.expect(TokenKind::Comma)?;
                    }
                }

                let end = self.chomp()?.span;

                let span = Span::merge(start, end);

                if let (ExprKind::IdentExpr(receiver), ExprKind::IdentExpr(method)) = (object.kind.clone(), member.kind) {
                    let kind = ExprKind::MethodCallExpr(MethodCallExpr {
                        receiver,
                        method,
                        args,
                    });

                    return Ok(Expr {
                        kind, 
                        span
                    });
                }
            } else {
                let end = member.span;

                let span = Span::merge(start, end);
                let kind = ExprKind::MemberExpr( MemberExpr {
                    receiver: object.into(),
                    member: member.into()
                });

                return Ok(Expr { 
                    kind, 
                    span 
                });
            }
        }

        Ok(object)
    }

    fn parse_index(&mut self) -> Result<Expr, CompileError> {
        let mut object = self.parse_array()?;

        while !self.tokens.is_empty() && (self.peek()?.kind == TokenKind::LeftBracket) {
            let start = self.chomp()?.span;

            // let mut args = Vec::new();

            // while !self.tokens.is_empty() && self.peek()?.kind != Token::RightParen {
            //     let arg = self.parse_expr();
            //     args.push(arg);
            //     if self.peek()?.kind != Token::RightParen {
            //         self.expect(Token::Comma);
            //     }
            // }

            let index = self.parse_expr()?;

            let end = self.chomp()?.span;

            let kind = ExprKind::IndexExpr(IndexExpr { 
                expr: Box::new(object), 
                index: Box::new(index)
            });
            let span = Span::merge(start, end);

            object = Expr { kind, span };
        }

        Ok(object)
    }

    fn parse_array(&mut self) -> Result<Expr, CompileError> {
        while !self.tokens.is_empty() && (self.peek()?.kind == TokenKind::LeftBracket) {
            let start = self.chomp()?.span;

            let mut elems = Vec::new();

            while !self.tokens.is_empty() && self.peek()?.kind != TokenKind::RightBracket {
                let arg = self.parse_expr()?;
                elems.push(arg);
                if self.peek()?.kind != TokenKind::RightBracket {
                    self.expect(TokenKind::Comma)?;
                }
            }

            let end = self.chomp()?.span;

            let kind = ExprKind::ArrExpr(ArrExpr { elems });
            let span = Span::merge(start, end);

            return Ok(Expr { kind, span });
        }

        self.parse_reference()
    }

    fn parse_reference(&mut self) -> Result<Expr, CompileError> {
        if !self.tokens.is_empty() && self.peek()?.kind == TokenKind::And {
            let start = self.chomp()?.span;
            let rvalue = self.parse_primary()?;
            let end = rvalue.span;

            let span = Span::merge(start, end);

            if let ExprKind::IdentExpr(ident) = rvalue.kind {
                let kind = ExprKind::ReferenceExpr(ident);
                Ok(Expr { 
                    kind, 
                    span 
                })
            } else {
                Err(CompileError::Other(format!("Expected an identifier REFERENCE {:?}", self.peek()?)))
            }

        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, CompileError> {
        let start = self.peek()?.span;

        let kind = match &self.peek()?.kind {
            TokenKind::Ident(ident) => ExprKind::IdentExpr(ident.to_owned()),
            TokenKind::Literal(lit) => ExprKind::LiteralExpr(lit.to_owned()),
            TokenKind::LeftParen => {
                self.chomp()?;
                let expr = self.parse_expr()?;

                if self.tokens.is_empty() || self.peek()?.kind != TokenKind::RightParen {
                    return Err(CompileError::UnbalancedToken(TokenKind::RightParen, expr.span.start));
                }

                expr.kind
            }

            n => return Err(CompileError::Other(format!("Unexpected expression {:?}", n))),
        };

        let end = self.chomp()?.span;
        let span = Span::merge(start, end);

        Ok(Expr { kind, span })
    }
}
