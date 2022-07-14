use serde::{Deserialize, Serialize};
use std::{error::Error, fmt::Display};

use crate::{
    lexer::{self, Lexem, Lexer},
    position::{self, Position},
};

#[derive(Debug, Clone, Serialize)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Operator::Add => "+",
            Operator::Sub => "-",
            Operator::Mul => "*",
            Operator::Div => "/",
            Operator::Eq => "=",
        };

        f.write_str(s)
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Literal::Integer(v) => format!("{}", v),
            Literal::Float(v) => format!("{}", v),
            Literal::Str(v) => format!("{}", v),
            Literal::Bool(v) => format!("{}", v),
        };

        f.write_str(s.as_str())
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum Expr {
    Void,
    Let {
        identifier: String,
        value: Box<Expr>,
        scope: Box<Expr>,
    },
    Lambda {
        arg: Box<Expr>,
        body: Box<Expr>,
    },
    Expression {
        operator: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Identifier {
        name: String,
    },
    Literal(Literal),
    Operator(Operator),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Expr::Void => "()".to_string(),
            Expr::Let {
                identifier,
                value,
                scope,
            } => format!("(let {} ({}) ({}))", identifier, value, scope),
            Expr::Lambda { arg, body } => format!("λ ({}) ({})", arg, body),
            Expr::Expression {
                operator,
                arguments,
            } => {
                let args = {
                    if arguments.len() == 0 {
                        "".to_string()
                    } else {
                        let fst = arguments.first().unwrap();

                        arguments
                            .iter()
                            .skip(1)
                            .fold(format!("{}", fst), |u, a| format!("{} {}", u, a))
                    }
                };

                format!("({} {})", operator, args)
            }
            Expr::Identifier { name } => name.to_owned(),
            Expr::Literal(l) => format!("{}", l),
            Expr::Operator(op) => format!("{}", op),
        };

        f.write_str(s.as_str())
    }
}

pub struct Parser;

#[derive(Debug)]
struct ParseResult<T> {
    expr: T,
    position: usize, // position from which to start parsing
}

macro_rules! token {
    ($tokens:expr, $position:expr) => {{
        $tokens
            .get($position)
            .ok_or(format!(
                "Unexpected end of tokens: position={}/{}",
                $position,
                $tokens.len() - 1
            ))
            .and_then(|(t, _)| {
                println!("[src/ast.rs:{}] {:?}", line!(), t);
                Ok(t)
            })
    }};
}

macro_rules! pos {
    ($tokens:expr, $position:expr) => {{
        let (_, p) = $tokens
            .get($position)
            .ok_or(format!(
                "Unexpected end of tokens: position={}/{}",
                $position,
                $tokens.len()
            ))
            .unwrap();
        p
    }};
}

impl Parser {
    fn operator_of_lexem(l: lexer::Operator) -> Expr {
        match l {
            lexer::Operator::Add => Expr::Operator(Operator::Add),
            lexer::Operator::Sub => Expr::Operator(Operator::Sub),
            lexer::Operator::Mul => Expr::Operator(Operator::Mul),
            lexer::Operator::Div => Expr::Operator(Operator::Div),
            lexer::Operator::Eq => Expr::Operator(Operator::Eq),
        }
    }

    fn parse_identifier(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        match token {
            Lexem::Identifier(id) => Ok(ParseResult {
                expr: Expr::Identifier {
                    name: id.to_owned(),
                },
                position: position + 1,
            }),
            _ => Err(format!("parse_identifier cannot parse {:?}", token)),
        }
    }

    fn parse_operator(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        if let Lexem::Operator(op) = token {
            Ok(ParseResult {
                expr: Parser::operator_of_lexem(*op),
                position: position + 1,
            })
        } else {
            Err(format!("parse_operator cannot parse {:?}", token))
        }
    }

    fn parse_argument_list(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Vec<Expr>>, String> {
        let mut expressions = vec![];
        let mut position = position;

        loop {
            let token = token!(tokens, position)?;

            match token {
                Lexem::Identifier(id) => expressions.push(Expr::Identifier {
                    name: id.to_owned(),
                }),
                Lexem::Parenthesis(lexer::PBState::Close) => {
                    break;
                }
                _ => {}
            }

            position += 1;
        }

        println!("[list] expressions: {:?}", expressions);

        Ok(ParseResult {
            expr: expressions,
            position: position + 1,
        })
    }

    fn parse_list(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Vec<Expr>>, String> {
        let mut expressions = vec![];
        let mut position = position;

        loop {
            match token!(tokens, position) {
                Ok(Lexem::Parenthesis(lexer::PBState::Close)) => {
                    break;
                }
                Ok(_) => {
                    let ParseResult {
                        expr,
                        position: new_position,
                    } = Parser::parse(tokens, position)?;

                    expressions.push(expr);
                    position = new_position;
                }
                Err(err) => return Err(err),
            }
        }

        println!("[list] expressions: {:?}", expressions);

        Ok(ParseResult {
            expr: expressions,
            position: position + 1,
        })
    }

    fn parse_literal(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        match token {
            Lexem::Literal(lit) => {
                let expr = match lit {
                    lexer::Literal::Integer(v) => Literal::Integer(*v),
                    lexer::Literal::Float(v) => Literal::Float(*v),
                    lexer::Literal::Str(v) => Literal::Str(v.to_owned()),
                    lexer::Literal::Bool(v) => Literal::Bool(*v),
                };

                return Ok(ParseResult {
                    expr: Expr::Literal(expr),
                    position: position + 1,
                });
            }
            _ => Err(format!("Invalid token in parse literal: {:?}", token)),
        }
    }

    fn parse_expr(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        if let Lexem::Parenthesis(lexer::PBState::Open) = token {
            let ParseResult {
                expr: operator,
                position: next_position,
            } = Parser::parse(tokens, position + 1)?;

            println!(
                "[expr :: operator] \n\t{}\n\tnext token: {:?}",
                serde_json::to_string_pretty(&operator).unwrap(),
                tokens.get(next_position)
            );

            let o = operator.clone();

            Parser::parse_list(tokens, next_position)
                .or(Ok(ParseResult {
                    expr: vec![],
                    position: next_position,
                }))
                .and_then(|r| {
                    let ParseResult {
                        expr: arguments,
                        position: next_position,
                    } = r;

                    if arguments.is_empty() {
                        println!(
                            "[expr] empty arguments, returning {:?}. Next token: {:?}",
                            operator,
                            tokens.get(next_position)
                        );

                        Ok(ParseResult {
                            expr: operator.clone(),
                            position: next_position,
                        })
                    } else {
                        Ok(ParseResult {
                            expr: Expr::Expression {
                                operator: Box::new(o),
                                arguments,
                            },
                            position: next_position,
                        })
                    }
                })
        } else {
            Err(format!(
                "parse_expr cannot parse an expression starting with {:?}",
                token
            ))
        }
    }

    fn parse_lambda(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        if let Lexem::Keyword(lexer::Keyword::Lambda) = token {
            let ParseResult {
                expr: arguments,
                position: args_position,
            } = Parser::parse_argument_list(tokens, position + 1)?;
            let ParseResult {
                expr: body,
                position: body_position,
            } = Parser::parse(tokens, args_position)?;

            let lambda = if arguments.is_empty() {
                Expr::Lambda {
                    arg: Box::new(Expr::Void),
                    body: Box::new(body),
                }
            } else {
                arguments.into_iter().rev().fold(body, |u, a| Expr::Lambda {
                    arg: Box::new(a),
                    body: Box::new(u),
                })
            };

            Ok(ParseResult {
                expr: lambda,
                position: body_position,
            })
        } else {
            Err(format!("parse_lambda cannot parse {:?}", token))
        }
    }

    fn parse_let(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        if let Lexem::Keyword(lexer::Keyword::Let) = token {
            Parser::parse_identifier(tokens, position + 1)
                .and_then(|r| match r {
                    ParseResult {
                        expr: Expr::Identifier { name },
                        position,
                    } => Ok((name, position)),
                    ParseResult { expr, position: _ } => {
                        Err(format!("Invalid variable identifier\"{:?}\" in let", expr))
                    }
                })
                .and_then(|(identifier, id_position)| {
                    println!(
                        "[let] parsing value:\n\tnext token: {:?}",
                        tokens.get(id_position)
                    );

                    let ParseResult {
                        expr: value,
                        position: val_position,
                    } = Parser::parse(tokens, id_position)?;

                    let ParseResult {
                        expr: scope,
                        position: scope_position,
                    } = Parser::parse(tokens, val_position)?;

                    Ok(ParseResult {
                        expr: Expr::Let {
                            identifier,
                            value: Box::new(value),
                            scope: Box::new(scope),
                        },
                        position: scope_position + 1,
                    })
                })
        } else {
            Err(format!("parse_let cannot parse {:?}", token))
        }
    }

    fn parse_keyword(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        match token {
            Lexem::Keyword(_) => Parser::parse_let(tokens, position)
                .or_else(|_| Parser::parse_lambda(tokens, position)),
            lex => Err(format!("parse_keyword cannot parse {:?}", lex)),
        }
    }

    fn parse_void(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        let token = token!(tokens, position)?;

        if let Lexem::Parenthesis(lexer::PBState::Open) = token {
            let token = token!(tokens, position + 1)?;
            if let Lexem::Parenthesis(lexer::PBState::Close) = token {
                Ok(ParseResult {
                    expr: Expr::Void,
                    position: position + 2,
                })
            } else {
                Err(format!("parse_void cannot parse {:?}", token))
            }
        } else {
            Err(format!("parse_void cannot parse {:?}", token))
        }
    }

    fn parse(
        tokens: &Vec<&(Lexem, Position)>,
        position: usize,
    ) -> Result<ParseResult<Expr>, String> {
        Parser::parse_expr(tokens, position)
            .or_else(|_| Parser::parse_void(tokens, position))
            .or_else(|_| Parser::parse_keyword(tokens, position))
            .or_else(|_| Parser::parse_identifier(tokens, position))
            .or_else(|_| Parser::parse_operator(tokens, position))
            .or_else(|_| Parser::parse_literal(tokens, position))
            .or(Err("Cannot parse empty source".to_string()))
    }

    pub fn run(tokens: &Vec<&(Lexem, Position)>) -> Result<Expr, String> {
        let r = Parser::parse(tokens, 0)?;

        Ok(r.expr)
    }
}
