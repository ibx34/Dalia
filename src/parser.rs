use std::{collections::HashMap, marker::PhantomData, string};

use crate::lexer::{Cursor, LexedResult, LexedResultType, Lexer};

#[derive(Debug, Clone)]
pub enum Definitions<'a> {
    Function {
        name: String,
        arg_ty_list: Vec<FnArg>,
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
    pub current_block: Option<usize>,
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
                arg_ty_list: Vec::new(),
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

#[derive(Debug, Clone)]
pub enum FnArg {
    Unnamed(Types),
    Named { name: String, ty: Types },
}

impl<'a> Parser<'a> {
    pub fn current_block_mut(&mut self) -> Result<&mut Block<'a>, String> {
        let Some(current_block) = self.current_block else {
            return Err(String::from("No current block"));
        };
        return Ok(self.get_block(current_block).unwrap());
    }

    pub fn get_block(&mut self, block_id: usize) -> Option<&mut Block<'a>> {
        if let Some(current_block) = self.current_block
            && current_block == block_id
        {
            return self.blocks.get_mut(&current_block);
        }
        return self.blocks.get_mut(&block_id);
    }

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
            expr: expr.map(Box::new),
            nested_in: match nest {
                Some(AutoNestSetting::NestUnder(under)) => Some(under),
                _ if let Ok(cb) = self.current_block_mut() => Some(cb.label),
                _ => None,
            },
        };
        if set_current {
            self.current_block = Some(new_block.label);
        }
        self.blocks.insert(new_block.label, new_block);
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
        if let Ok(cb) = self.current_block_mut() {
            if let Some(symbol) = cb.symbols.symbols.get(symbol.as_str()) {
                return Ok((*symbol).to_owned());
            } else if let Some(nested_in) = cb.nested_in {
                return self.get_symbol(symbol, Some(nested_in));
            }
        }
        if let Some(symbol) = self.symbols.symbols.get(symbol.as_str()) {
            return Ok((*symbol).to_owned());
        }
        Err(String::from("Could not get symbol... like at all"))
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
    pub fn parse_arg_list(
        &mut self,
        symbols: &mut SymbolTable<'a>,
        end_at: LexedResultType<'a>,
    ) -> Result<Vec<FnArg>, String> {
        let mut args = Vec::new();
        while let Ok(peeked) = self.cursor.peek() {
            let peeked: LexedResult<'a> = (*peeked).to_owned();
            if let Ok(true) = self.peek_then_advance(LexedResultType::Dash)
                && let Ok(true) = self.peek_then_advance(LexedResultType::GreaterThan)
            {
                continue;
            } else if peeked.ty == end_at {
                self.cursor.advance(1)?;
                return Ok(args);
            }
            if let Ok(ty) = self.parse_ty() {
                self.cursor.advance(1)?;
                args.push(FnArg::Unnamed(ty));
            } else if let LexedResultType::IdentLiteral(ident) = peeked.ty {
                let ident = ident.to_string();
                self.cursor.advance(1)?;

                let Ok(r#type) = &self.parse_ty() else {
                    panic!("Expected types in the function argument list.")
                };
                self.cursor.advance(1)?;

                symbols
                    .symbols
                    .insert(ident.to_owned(), Expr::Type(r#type.to_owned()));
                args.push(FnArg::Named {
                    name: ident,
                    ty: r#type.to_owned(),
                });
            }
        }
        Ok(args)
    }

    pub fn parse_fn_application(&mut self, fn_def: Expr<'a>) -> ParseFnResult<'a> {
        if let Ok(LexedResult { ty: LexedResultType::OpenP , .. }) = self.cursor.current() {
            println!("Past openP?");
            self.cursor.advance(1)?;

            if let Expr::Definitions(Definitions::Function {
                name,
                ..
            }) = fn_def
            {
                println!("Good definition? (c:{:?})", self.cursor.current());
                // println!("__{:?}", self.cursor.advance_ret(1)?);
                let mut app_args = Vec::new();
                while let Ok(expr) = self.cursor.peek() {
                    if expr.ty == LexedResultType::CloseP {
                        println!("BROKEN @ {:?}", self.cursor.current());
                        self.cursor.advance(1)?;
                        break;
                    }
                    let parsed = self.parse()?;
                    app_args.push(Box::new(parsed));
                }
                return Ok(Expr::Application {
                    function: name,
                    arguments: app_args,
                });
            }
        }
        Err(String::from("Bad definition for fn call."))
    }

    pub fn parse_fn_definition(&mut self, ident: String) -> ParseFnResult<'a> {
        let mut symbol_table = SymbolTable {
            symbols: HashMap::new(),
        };
        let lhs = self.parse_arg_list(&mut symbol_table, LexedResultType::Eq)?;
        self.push_new_block(None, true, None, Some(symbol_table))?;
        println!("fn {ident:?} (c:{:?})", self.cursor.current());
        let rhs = self.parse()?;
        self.current_block_mut()?.expr = Some(Box::new(rhs.to_owned()));
        println!("finished {ident:?}");
        return Ok(Expr::Definitions(Definitions::Function {
            name: ident.to_owned(),
            arg_ty_list: lhs,
            return_ty: None,
            body: self.current_block_mut()?.label,
            phantom: PhantomData,
        }));
    }

    pub fn parse_ty(&mut self) -> Result<Types, String> {
        let current = self.cursor.current()?;

        match &current.ty {
            LexedResultType::IdentLiteral(ident) => {
                let ident = ident.to_string();
                if let Expr::Type(r#type) = self.get_symbol(ident.to_owned(), None)? {
                    return Ok(r#type);
                }
                todo!("{:?}", ident.to_owned());
            }
            LexedResultType::OpenP => {
                self.cursor.advance(1)?;
                if let LexedResultType::CloseP = self.cursor.peek()?.ty {
                    return Ok(Types::PlaceholderTuple);
                } else {
                    // this table is a useless allocation. ill refactor
                    // this out when i rewrite the parse_arg_list fn
                    let mut symbol_table = SymbolTable {
                        symbols: HashMap::new(),
                    };
                    let arg_list = self
                        .parse_arg_list(&mut symbol_table, LexedResultType::CloseP)?
                        .into_iter()
                        .map(|e| {
                            Box::new(match e {
                                FnArg::Unnamed(ty) => ty,
                                FnArg::Named { ty, .. } => ty,
                            })
                        })
                        .collect::<Vec<Box<Types>>>();
                    return Ok(Types::FunctionType {
                        arg_ty_list: arg_list,
                        return_ty: None,
                    });
                }
            }
            a @ _ => panic!("{:?}", a),
        }
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
                    println!("!!! {:?}", definition);
                    return Ok(definition);
                } else if let Ok(symbol) = self.get_symbol(ident.to_string(), None) {
                    match symbol {
                        a @ Expr::Definitions(Definitions::Function { .. }) => {
                            println!("Fn application");
                            return self.parse_fn_application(a.to_owned())
                        }
                        Expr::Type(ty) if let Some(cb) = self.current_block => {
                            println!("{ty:?}  (c:{:?}) Should be a type reference?", self.cursor.current());
                            return Ok(Expr::Reference {
                                name: ident.to_string(),
                                ty,
                                block: cb,
                            })
                        }
                        _ => {
                            println!("We shouldnt be here...");
                            return Ok(symbol.to_owned())
                        },
                    }
                }
                panic!("Unahndled circumstances {:?}", ident.to_string())
            }
            a => todo!("{:?}", a),
        }
    }
}
