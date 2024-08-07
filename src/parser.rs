use std::{borrow::Cow, collections::HashMap, marker::PhantomData};

use crate::lexer::{Cursor, LexedResult, LexedResultType, Lexer};

#[derive(Debug, Clone)]
pub enum Definitions<'a> {
    Function {
        name: String,
        arg_ty_list: HashMap<String, Types>,
        return_ty: Option<Types>,
        body: Box<Expr<'a>>,
        phantom: PhantomData<&'a ()>
    },
}

#[derive(Debug, Clone)]
pub enum Types {
    Primitive(PrimitiveTypes),
    FunctionType {
        arg_ty_list: Vec<Box<Types>>,
        return_ty: Option<Box<Types>>,
    },
}

#[derive(Debug, Clone)]
pub enum PrimitiveTypes {
    Str,
    Int,
}

impl TryFrom<&str> for PrimitiveTypes {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "Str" => Ok(PrimitiveTypes::Str),
            "Int" => Ok(PrimitiveTypes::Int),
            _ => Err(String::from("Unkown primitive")),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Definitions(Definitions<'a>),
    Application {
        function: String,
        arguments: Vec<String>,
    },
    Type(Types),
    Block {
        symbols: HashMap<&'a str, Box<Expr<'a>>>,
        expr: Box<Expr<'a>>
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub cursor: Cursor<LexedResult<'a>>,
    pub symbols: HashMap<&'a str, Expr<'a>>,
}

// This is just nicer for me, but, for APIs in the future
// might be better to follow the .init() style.
impl<'a> From<Lexer<'a>> for Parser<'a> {
    fn from(value: Lexer<'a>) -> Parser<'a> {
        let mut symbols = HashMap::new();
        symbols.insert("Str", Expr::Type(Types::Primitive(PrimitiveTypes::Str)));
        Parser {
            symbols,
            cursor: Cursor::init(value.results),
        }
    }
}

impl<'a> Parser<'a> {
    pub fn peek_then_advance(&mut self, expected: LexedResultType) -> Result<bool, String> {
        let peeked = self.cursor.peek()?;
        if peeked.ty == expected {
            self.cursor.advance(1)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    /// Expects to start immediatly after the ::
    pub fn parse_fn_type(&mut self) -> Result<HashMap<String, Types>, String> {
        let mut fn_arg_list = HashMap::new();
        while let Ok(peeked) = self.cursor.peek() {
            let peeked = (*peeked).to_owned();
            if let Ok(true) = self.peek_then_advance(LexedResultType::Dash)
                && let Ok(true) = self.peek_then_advance(LexedResultType::GreaterThan)
            {
                continue;
            } else if peeked.ty == LexedResultType::Eq {
                self.cursor.advance(1)?;
                break;
            } else if let LexedResultType::IdentLiteral(ident) = &peeked.ty {
                let ident: Cow<str> = ident.to_owned();
                if let Some(Expr::Type(r#type)) = self.symbols.get(ident.as_ref()) {
                    self.cursor.advance(1)?;
                    fn_arg_list.insert(String::from("ret"), r#type.to_owned());
                } else if let Ok(()) = self.cursor.advance(1)
                    && let Ok(Expr::Type(r#type)) = self.parse()
                {
                    fn_arg_list.insert(ident.to_string(), r#type);
                } else {
                    panic!("Expected either 1. ident followed by type, or 2. a type alone")
                }
            }
        }
        Ok(fn_arg_list)
    }

    pub fn parse_fn_definition(&mut self, ident: String) -> Result<Expr<'a>, String> {
        let rhs = self.parse_fn_type()?;
        let lhs = self.parse()?;
        
        return Ok(Expr::Definitions(Definitions::Function {
            name: ident.to_owned(),
            arg_ty_list: rhs,
            return_ty: None,
            body: Box::new(lhs),
            phantom: PhantomData
        }));
    }

    pub fn parse(&mut self) -> Result<Expr<'a>, String> {
        let current = self.cursor.current()?;
        return Ok(match &current.ty {
            LexedResultType::IdentLiteral(ident) => {
                let ident = ident.to_owned();
                self.cursor.advance(1)?;

                if let Ok(true) = self.peek_then_advance(LexedResultType::Colon)
                    && let Ok(true) = self.peek_then_advance(LexedResultType::Colon)
                {
                    return self.parse_fn_definition(ident.to_string())
                } else if let Some(symbol) = self.symbols.get(ident.as_ref()) {
                    return Ok((*symbol).to_owned());
                }
                panic!("Unahndled circumstances {:?}", ident.to_string())
            }
            _ => todo!(),
        });
    }
}
