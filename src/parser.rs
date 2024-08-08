use std::{borrow::Cow, collections::HashMap, hash::Hash, marker::PhantomData};

use crate::lexer::{Cursor, LexedResult, LexedResultType, Lexer};

#[derive(Debug, Clone)]
pub enum Definitions<'a> {
    Function {
        name: String,
        arg_ty_list: HashMap<String, Types>,
        return_ty: Option<Types>,
        /// References a specific block that contains the body
        /// of this function
        body: usize,
        phantom: PhantomData<&'a ()>,
    },
}

#[derive(Debug, Clone)]
pub enum Types {
    Primitive(PrimitiveTypes),
    ArgumentListType {
        ident: String,
        ty: Box<Types>,
    },
    PlaceholderTuple,
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
pub struct SymbolTable<'a> {
    symbols: HashMap<String, Expr<'a>>,
}

#[derive(Debug, Clone)]
pub struct Block<'a> {
    label: usize,
    symbols: SymbolTable<'a>,
    expr: Option<Box<Expr<'a>>>,
    nested_in: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Definitions(Definitions<'a>),
    Application {
        function: String,
        arguments: Vec<Box<Expr<'a>>>,
    },
    Type(Types),
    Block(Block<'a>),
    Reference {
        name: String,
        ty: Types,
        block: usize,
    },
    Dud,
}

type ParseFnResult<'a> = Result<Expr<'a>, String>;

#[derive(Debug)]
pub struct Parser<'a> {
    pub cursor: Cursor<LexedResult<'a>>,
    pub symbols: SymbolTable<'a>,
    pub block_inc_c: usize,
    pub blocks: HashMap<usize, Block<'a>>,
    pub current_block: Option<Block<'a>>,
}

// This is just nicer for me, but, for APIs in the future
// might be better to follow the .init() style.
impl<'a> From<Lexer<'a>> for Parser<'a> {
    fn from(value: Lexer<'a>) -> Parser<'a> {
        let mut symbols = HashMap::new();
        symbols.insert(
            "Str".to_string(),
            Expr::Type(Types::Primitive(PrimitiveTypes::Str)),
        );
        symbols.insert(
            "Int".to_string(),
            Expr::Type(Types::Primitive(PrimitiveTypes::Int)),
        );
        let mut print_args = HashMap::new();
        print_args.insert(
            String::from("to_print"),
            Types::Primitive(PrimitiveTypes::Str),
        );
        symbols.insert(
            "print".to_string(),
            Expr::Definitions(Definitions::Function {
                name: String::from("print"),
                arg_ty_list: HashMap::new(),
                return_ty: None,
                body: 0,
                phantom: PhantomData,
            }),
        );
        Parser {
            symbols: SymbolTable { symbols },
            blocks: HashMap::new(),
            block_inc_c: 0,
            current_block: None,
            cursor: Cursor::init(value.results),
        }
    }
}

pub enum AutoNestSetting {
    AutoNestCurrent,
    NestUnder(usize),
}

impl<'a> Parser<'a> {
    // function defaults to pushing current unless specified otherwise.
    // [nest]: Defauls to nesting under current_block
    pub fn push_new_block(
        &mut self,
        expr: Option<Expr<'a>>,
        set_current: bool,
        nest: Option<AutoNestSetting>,
        symbol_table: Option<SymbolTable<'a>>,
    ) -> Result<(), String> {
        let block_name = self.block_inc_c;
        self.block_inc_c += 1;
        let symbols = symbol_table.unwrap_or(SymbolTable {
            symbols: HashMap::new(),
        });
        let new_block = Block {
            label: block_name,
            symbols,
            expr: expr.map(|e| Box::new(e)),
            nested_in: match nest {
                Some(AutoNestSetting::NestUnder(under)) => Some(under),
                _ if let Some(ref cb) = self.current_block => Some(cb.label),
                _ => None,
            },
        };
        if set_current && let Some(cb) = self.current_block.to_owned() {
            let cb = cb.to_owned();
            self.current_block = Some(new_block);
            self.blocks.insert(cb.label, cb);
        } else {
            self.current_block = Some(new_block);
        }
        Ok(())
    }

    // This function will attempt to get the symbol from the CURRENT BLOCK
    // or default to the global symbol table where function defs, and such,
    // live.
    pub fn get_symbol(
        &mut self,
        symbol: String,
        look_in_block: Option<usize>,
    ) -> ParseFnResult<'a> {
        if let Some(block_id) = look_in_block
            && let Some(block) = self.blocks.get(&block_id)
        {
            let Some(symbol) = block.symbols.symbols.get(symbol.as_str()) else {
                return Err(String::from("Could not get symbol from specified table"));
            };
            return Ok((*symbol).to_owned());
        }
        if let Some(cb) = &self.current_block {
            if let Some(symbol) = cb.symbols.symbols.get(symbol.as_str()) {
                return Ok((*symbol).to_owned());
            } else if let Some(nested_in) = cb.nested_in {
                return Ok(self.get_symbol(symbol, Some(nested_in))?);
            }
        }
        if let Some(symbol) = self.symbols.symbols.get(symbol.as_str()) {
            return Ok((*symbol).to_owned());
        }
        return Err(String::from("Could not get symbol... like at all"));
    }

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
    pub fn parse_fn_type(
        &mut self,
        symbols: &mut SymbolTable<'a>,
    ) -> Result<HashMap<String, Types>, String> {
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
            }

            let parsed = self.parse()?;
            match parsed {
                Expr::Type(Types::ArgumentListType { ident, ty }) => {
                    symbols
                        .symbols
                        .insert(ident.to_string(), Expr::Type(*ty.to_owned()));
                    fn_arg_list.insert(ident.to_string(), *ty);
                }
                Expr::Type(a) => {
                    fn_arg_list.insert(String::from("ret"), a);
                }
                _ => panic!("Unexpected type..."),
            }
        }
        Ok(fn_arg_list)
    }

    pub fn parse_fn_application(&mut self, fn_def: Expr<'a>) -> ParseFnResult<'a> {
        // if let Expr::Definitions(Definitions::Function {
        //     name,
        //     arg_ty_list,
        //     return_ty,
        //     body,
        //     phantom,
        // }) = fn_def
        // {
        //     // TODO: We want paranthesis to be optional, not mandatory.
        //     // that is for the self-compiled iteration!
        //     if self.cursor.current()?.ty != LexedResultType::OpenP {
        //         return Err(String::from("Expected open paran"));
        //     }
        //     self.cursor.advance(1)?;
        //     let mut app_args = Vec::new();
        //     while let Ok(expr) = self.cursor.peek() {
        //         if expr.ty == LexedResultType::CloseP {
        //             self.cursor.advance(1)?;
        //             break;
        //         }
        //         let parsed = self.parse()?;
        //         app_args.push(Box::new(Expr::Ref));
        //         self.cursor.advance(1)?;
        //     }

        //     return Ok(Expr::Application {
        //         function: name,
        //         arguments: app_args,
        //     });
        // }
        todo!()
    }

    pub fn parse_fn_definition(&mut self, ident: String) -> ParseFnResult<'a> {
        let mut symbol_table = SymbolTable {
            symbols: HashMap::new(),
        };
        let rhs = self.parse_fn_type(&mut symbol_table)?;

        self.push_new_block(None, true, None, Some(symbol_table))?;
        let lhs = self.parse()?;
        self.current_block.as_mut().unwrap().expr = Some(Box::new(lhs.to_owned()));

        return Ok(Expr::Definitions(Definitions::Function {
            name: ident.to_owned(),
            arg_ty_list: rhs,
            return_ty: None,
            body: self.current_block.as_ref().unwrap().label,
            phantom: PhantomData,
        }));
    }

    pub fn parse(&mut self) -> ParseFnResult<'a> {
        let current = self.cursor.current()?;
        match &current.ty {
            LexedResultType::IdentLiteral(ident) => {
                let ident = ident.to_owned();
                self.cursor.advance(1)?;

                if let Ok(true) = self.peek_then_advance(LexedResultType::Colon)
                    && let Ok(true) = self.peek_then_advance(LexedResultType::Colon)
                {
                    let ident_str = ident.to_string();
                    let definition = self.parse_fn_definition(ident_str.to_owned())?;
                    self.symbols
                        .symbols
                        .insert(ident_str.to_owned(), definition.to_owned());
                    return Ok(definition);
                } else if let Ok(symbol) = self.get_symbol(ident.to_string(), None) {
                    match symbol {
                        a @ Expr::Definitions(Definitions::Function { .. }) => {
                            return Ok(self.parse_fn_application(a.to_owned())?)
                        }
                        _ => return Ok(symbol.to_owned()),
                    }
                } else if let Ok(Expr::Type(r#type)) = self.parse() {
                    return Ok(Expr::Type(Types::ArgumentListType {
                        ident: ident.to_string(),
                        ty: Box::new(r#type),
                    }));
                }
                panic!("Unahndled circumstances {:?}", ident.to_string())
            }
            LexedResultType::OpenP => {
                self.cursor.advance(1)?;
                if let Ok(LexedResult {
                    ty: LexedResultType::CloseP,
                    ..
                }) = self.cursor.current()
                {
                    self.cursor.advance(1)?;
                    return Ok(Expr::Type(Types::PlaceholderTuple));
                }
                todo!()
            }
            a @ _ => todo!("{:?}", a),
        }
    }
}
