use std::{fmt::Debug, iter::Peekable, vec};

use crate::position::Position;

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Let,
    Lambda,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
}

#[derive(Debug, PartialEq, Eq)]
pub enum PBState {
    Open,
    Close,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Eq
}

#[derive(Debug, PartialEq)]
pub enum Lexem {
    Keyword(Keyword),
    Literal(Literal),
    Identifier(String),
    Parenthesis(PBState),
    Operator(Operator),
    Comma,
}

impl Lexem {
    fn parse(token: &String, position: Position) -> (Self, Position) {
        let is_float = {
            let has_numeric_parts = token
                .matches(|c: char| c.is_numeric())
                .collect::<Vec<&str>>()
                .len()
                == 2;

            let has_single_sep = token.chars().filter(|&c| c == '.').count() == 1;

            has_numeric_parts && has_single_sep
        };

        let is_int = token.chars().filter(|&c| c.is_numeric()).count() == token.len();

        let id_segments = token
            .matches(|c: char| c.is_alphanumeric() || ("_-".contains(c)))
            .collect::<Vec<&str>>();
        let is_identifier = id_segments.len() == token.len();

        println!(
            "\"{}\": is_float: {}, is_int: {}, is_identifier: {} ({:?})",
            token, is_float, is_int, is_identifier, id_segments
        );

        match token.as_str() {
            "λ" | "lambda" | "fn" => (Lexem::Keyword(Keyword::Lambda), position),
            "let" => (Lexem::Keyword(Keyword::Let), position),
            "+" => (Lexem::Operator(Operator::Add), position),
            "-" => (Lexem::Operator(Operator::Sub), position),
            "*" | "·" | "×" => (Lexem::Operator(Operator::Mul), position),
            "/" => (Lexem::Operator(Operator::Div), position),
            "=" => (Lexem::Operator(Operator::Eq), position),
            "true" => (Lexem::Literal(Literal::Bool(true)), position),
            "false" => (Lexem::Literal(Literal::Bool(false)), position),
            token if is_int => (
                Lexem::Literal(Literal::Integer(token.parse::<i64>().unwrap())),
                position,
            ),
            token if is_float => (
                Lexem::Literal(Literal::Float(token.parse::<f64>().unwrap())),
                position,
            ),
            token if is_identifier => (Lexem::Identifier(token.to_owned()), position),
            _ => todo!("'{}'", token),
        }
    }
}

pub struct Lexer {
    lexems: Vec<(Lexem, Position)>,
    source: String,
    token: String,
    current_position: Position,
    token_start: Position,
    escape: Option<char>,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lexer")
            .field("lexems", &self.lexems)
            .field("token", &self.token)
            .field("current_position", &self.current_position)
            .field("token_start", &self.token_start)
            .field("escape", &self.escape)
            .finish()
    }
}

impl Lexer {
    pub fn new(string: String) -> Self {
        Lexer {
            lexems: vec![],
            source: string,
            token: String::new(),
            current_position: Position::new(0, 0),
            token_start: Position::new(0, 0),
            escape: None,
        }
    }

    fn is_alone(&self, c: &char) -> bool {
        "()[]{}λ,".contains(*c)
    }

    pub fn lex(&mut self) {
        let mut stream = self.source.chars().peekable();

        while let Some(c) = stream.next() {
            self.current_position.next_column();

            if self.token.is_empty() {
                self.token_start = self.current_position;
            }

            if (self.is_alone(&c) || c.is_whitespace()) && self.escape == None {
                if !self.token.is_empty() {
                    self.lexems
                        .push(Lexem::parse(&self.token, self.token_start));
                    self.token = String::new();
                }
            }

            if c == '\n' {
                self.current_position.next_row();
                self.token_start = self.current_position;
            }

            if c.is_whitespace() {
                self.token_start = self.current_position;
                continue;
            }

            // loners
            match c {
                // Parenthesis
                '(' => self
                    .lexems
                    .push((Lexem::Parenthesis(PBState::Open), self.token_start)),
                ')' => self
                    .lexems
                    .push((Lexem::Parenthesis(PBState::Close), self.token_start)),
                // Keywords
                'λ' => self
                    .lexems
                    .push((Lexem::Keyword(Keyword::Lambda), self.token_start)),
                // inter
                ',' => self.lexems.push((Lexem::Comma, self.token_start)),
                _ if (!c.is_whitespace() && self.escape != Some('"')) => self.token.push(c),
                _ => {}
            }
        }

        if !self.token.is_empty() {
            self.lexems
                .push(Lexem::parse(&self.token, self.token_start));
            self.token = String::new();
        }
    }

    pub fn lexems(&self) -> Vec<&(Lexem, Position)> {
        let mut out = vec![];

        for l in self.lexems.iter() {
            out.push(l);
        }

        out
    }
}
