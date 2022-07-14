use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
};

use serde::Serialize;

use crate::ast::{Expr, Literal};

#[derive(Debug, Serialize)]
pub struct Frame {
    bindings: HashMap<String, Box<Expr>>,
}

pub struct Evaluator {
    stack: VecDeque<Frame>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            stack: VecDeque::new(),
        }
    }

    pub fn with_stack(stack: VecDeque<Frame>) -> Self {
        Self { stack }
    }

    fn lookup(&self, id: &String) -> Option<&Box<Expr>> {
        self.stack
            .iter()
            .find(|f| f.bindings.contains_key(id))
            .and_then(|f| f.bindings.get(id))
    }

    pub fn eval(&mut self, expr: Expr) -> Result<Expr, String> {
        match expr {
            Expr::Void => {
                return Ok(expr);
            }
            Expr::Let {
                identifier,
                value,
                scope,
            } => {
                let mut frame = Frame {
                    bindings: HashMap::new(),
                };

                frame.bindings.insert(identifier, value);
                self.stack.push_front(frame);

                let result = self.eval(*scope);

                self.stack.pop_front();
                return result;
            }
            Expr::Lambda { arg, body } => Ok(Expr::Lambda { arg, body }),
            Expr::Expression {
                operator,
                arguments,
            } => match *operator {
                Expr::Void => return Ok(Expr::Void),
                Expr::Lambda { arg, body } => {
                    if arguments.len() == 0 {
                        return Ok(Expr::Lambda { arg, body })
                    } else {
                        let arg_value = arguments.first().unwrap().clone();

                        match (*arg, arg_value) {
                            (Expr::Void, Expr::Void) => todo!(),
                            (Expr::Identifier { name }, arg_value) => {
                                let mut frame = Frame {
                                    bindings: HashMap::new(),
                                };
                
                                frame.bindings.insert(name, Box::new(arg_value.clone()));
                                self.stack.push_front(frame);
                
                                let new_operator = self.eval(*body)?;

                                let result = self.eval(
                                    if arguments.len() == 1 {
                                        new_operator
                                    } else {
                                        Expr::Expression { 
                                            operator: Box::new(new_operator), 
                                            arguments: arguments.iter().skip(1).map(|e| e.clone()).collect::<Vec<Expr>>() 
                                        }
                                    }
                                    );
                
                                self.stack.pop_front();
                                return result;
                            }
                            _ => todo!()
                        }
                    }
                },
                Expr::Expression {
                    operator,
                    arguments,
                } => {
                    let e = self.eval(*operator)?;

                    self.eval(Expr::Expression {
                        operator: Box::new(e),
                        arguments,
                    })
                }
                Expr::Identifier { name } => {
                    let l = self.lookup(&name);

                    if let Some(v) = l {
                        let new_expr = Expr::Expression {
                            operator: Box::new(*v.clone()),
                            arguments,
                        };

                        return self.eval(new_expr);
                    } else {
                        return Err(format!(
                            "{} not defined in the current scope ({:?})",
                            name, self.stack
                        ));
                    }
                },
                Expr::Literal(Literal::Bool(b)) => {
                    if arguments.len() == 0 {
                        return Ok(Expr::Literal(Literal::Bool(b)))
                    } else {
                        if b {
                            return self.eval(arguments.first().unwrap().to_owned())
                        } else {
                            return self.eval(arguments.get(1).unwrap_or(&Expr::Void).to_owned())
                        }
                    }
                },
                Expr::Operator(op) => {
                    if arguments.len() == 0 {
                        return Ok(Expr::Operator(op))
                    } else {
                        let first = self.eval(arguments.first().unwrap().clone()).unwrap();
                        let rest = arguments.to_owned().into_iter().skip(1);

                        let result = match op {
                            crate::ast::Operator::Add => 
                                rest.fold(Ok(first.to_owned()), |u, a| {
                                    if let Ok(u) = u  {
                                        let a_eval = self.eval(a).unwrap();

                                    match (u, a_eval) {
                                        (Expr::Literal(Literal::Bool(u)), Expr::Literal(Literal::Bool(a))) => 
                                            Ok(Expr::Literal(Literal::Bool(u || a))),
                                        (Expr::Literal(Literal::Float(u)), Expr::Literal(Literal::Float(a))) =>
                                            Ok(Expr::Literal(Literal::Float(u + a))),
                                        (Expr::Literal(Literal::Integer(u)), Expr::Literal(Literal::Integer(a))) =>
                                            Ok(Expr::Literal(Literal::Integer(u + a))),
                                        (Expr::Literal(Literal::Str(u)), Expr::Literal(Literal::Str(a))) =>
                                            Ok(Expr::Literal(Literal::Str(format!("{}{}", u, a)))),
                                        (u, a) => Err(format!("Mismatched argument types in {}: {}, {}. Argument types must be the same.", op, u, a))
                                    }
                                    } else {
                                        u
                                    }
                                })
                            ,
                            crate::ast::Operator::Sub => {
                                rest.fold(Ok(first.to_owned()), |u, a| {
                                    if let Ok(u) = u  {
                                        let a_eval = self.eval(a).unwrap();

                                    match (u, a_eval) {
                                        (Expr::Literal(Literal::Bool(u)), Expr::Literal(Literal::Bool(a))) => 
                                            Ok(Expr::Literal(Literal::Bool(u ^ a))),
                                        (Expr::Literal(Literal::Float(u)), Expr::Literal(Literal::Float(a))) =>
                                            Ok(Expr::Literal(Literal::Float(u - a))),
                                        (Expr::Literal(Literal::Integer(u)), Expr::Literal(Literal::Integer(a))) =>
                                            Ok(Expr::Literal(Literal::Integer(u - a))),
                                        (Expr::Literal(Literal::Str(u)), Expr::Literal(Literal::Str(a))) =>
                                            Ok(Expr::Literal(Literal::Str(u.clone().trim_matches(|c| {a.contains(c)}).to_string()))),
                                        (u, a) => Err(format!("Mismatched argument types in {}: {}, {}. Argument types must be the same.", op, u, a))
                                    }
                                    } else {
                                        u
                                    }
                                })
                            },
                            crate::ast::Operator::Mul => {
                                rest.fold(Ok(first.to_owned()), |u, a| {
                                    if let Ok(u) = u  {
                                        let a_eval = self.eval(a).unwrap();

                                    match (u, a_eval) {
                                        (Expr::Literal(Literal::Bool(u)), Expr::Literal(Literal::Bool(a))) => 
                                            Ok(Expr::Literal(Literal::Bool(u && a))),
                                        (Expr::Literal(Literal::Float(u)), Expr::Literal(Literal::Float(a))) =>
                                            Ok(Expr::Literal(Literal::Float(u * a))),
                                        (Expr::Literal(Literal::Integer(u)), Expr::Literal(Literal::Integer(a))) =>
                                            Ok(Expr::Literal(Literal::Integer(u * a))),
                                        (Expr::Literal(Literal::Str(u)), Expr::Literal(Literal::Str(a))) =>
                                            Err(format!("Cannot multiply two string: \"{}\", \"{}\"", u, a)),
                                            // Ok(Expr::Literal(Literal::Str(u.clone().trim_matches(|c| {a.contains(c)}).to_string()))),
                                        (u, a) => Err(format!("Mismatched argument types in {}: {}, {}. Argument types must be the same.", op, u, a))
                                    }
                                    } else {
                                        u
                                    }
                                })
                            },
                            crate::ast::Operator::Div => {
                                rest.fold(Ok(first.to_owned()), |u, a| {
                                    if let Ok(u) = u  {
                                        let a_eval = self.eval(a).unwrap();

                                    match (u, a_eval) {
                                        (Expr::Literal(Literal::Bool(u)), Expr::Literal(Literal::Bool(a))) => 
                                            Ok(Expr::Literal(Literal::Bool(u & a))),
                                        (Expr::Literal(Literal::Float(u)), Expr::Literal(Literal::Float(a))) =>
                                            Ok(Expr::Literal(Literal::Float(u / a))),
                                        (Expr::Literal(Literal::Integer(u)), Expr::Literal(Literal::Integer(a))) =>
                                            Ok(Expr::Literal(Literal::Integer(u / a))),
                                        (Expr::Literal(Literal::Str(u)), Expr::Literal(Literal::Str(a))) =>
                                            Err(format!("Cannot divide two string: \"{}\", \"{}\"", u, a)),
                                            // Ok(Expr::Literal(Literal::Str(u.clone().trim_matches(|c| {a.contains(c)}).to_string()))),
                                        (u, a) => Err(format!("Mismatched argument types in {}: {}, {}. Argument types must be the same.", op, u, a))
                                    }
                                    } else {
                                        u
                                    }
                                })
                            },
                            crate::ast::Operator::Eq => {
                                rest.fold(Ok(first.to_owned()), |u, a| {
                                    if let Ok(u) = u  {
                                        let a_eval = self.eval(a).unwrap();

                                    match (u, a_eval) {
                                        (Expr::Literal(Literal::Bool(u)), Expr::Literal(Literal::Bool(a))) => 
                                            Ok(Expr::Literal(Literal::Bool(u == a))),
                                        (Expr::Literal(Literal::Float(u)), Expr::Literal(Literal::Float(a))) =>
                                            Ok(Expr::Literal(Literal::Bool(u == a))),
                                        (Expr::Literal(Literal::Integer(u)), Expr::Literal(Literal::Integer(a))) =>
                                            Ok(Expr::Literal(Literal::Bool(u == a))),
                                        (Expr::Literal(Literal::Str(u)), Expr::Literal(Literal::Str(a))) =>
                                            Ok(Expr::Literal(Literal::Bool(u == a))),
                                        (u, a) => Err(format!("Mismatched argument types in {}: {}, {}. Argument types must be the same.", op, u, a))
                                    }
                                    } else {
                                        u
                                    }
                                })
                            },
                        };

                        return Ok(result?)
                    }
                },
                _ => Err(format!("Invalid expression operator {}", operator)),
            },
            Expr::Identifier { name } => {
                let l = self.lookup(&name);

                if let Some(v) = l {
                    let new_expr = *v.clone();

                    return self.eval(new_expr);
                } else {
                    return Err(format!(
                        "{} not defined in the current scope ({})",
                        name, serde_json::to_string(&self.stack).unwrap()
                    ));
                }
            }
            Expr::Literal(v) => Ok(Expr::Literal(v)),
            Expr::Operator(op) => Ok(Expr::Operator(op)),
        }
    }
}
