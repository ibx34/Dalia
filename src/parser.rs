use std::{collections::HashMap, marker::PhantomData, process::id};

use crate::lexer::{Cursor, LexedResult, LexedResultType, Lexer};

#[derive(Debug)]
pub struct Expr {
    
}

#[derive(Debug)]
pub struct Parser<'a> {
    pub cursor: Cursor<LexedResult<'a>>,
}

// #[derive(Debug, Clone)]
// pub enum Definitions<'a> {
//     Function {
//         name: String,
//         arg_ty_list: Vec<FnArg>,
//         return_ty: Option<Types>,
//         /// References a specific block that contains the body
//         /// of this function
//         body: usize,
//         phantom: PhantomData<&'a ()>,
//     },
// }

// #[derive(Debug, Clone)]
// pub enum Types {
//     Primitive(PrimitiveTypes),
//     ArgumentListType {
//         ident: String,
//         ty: Box<Types>,
//     },
//     PlaceholderTuple,
//     FunctionType {
//         arg_ty_list: Vec<Box<Types>>,
//         return_ty: Option<Box<Types>>,
//     },
// }

// #[derive(Debug, Clone)]
// pub enum PrimitiveTypes {
//     Str,
//     Int,
// }

// impl TryFrom<&str> for PrimitiveTypes {
//     type Error = String;

//     fn try_from(value: &str) -> Result<Self, Self::Error> {
//         match value {
//             "Str" => Ok(PrimitiveTypes::Str),
//             "Int" => Ok(PrimitiveTypes::Int),
//             _ => Err(String::from("Unkown primitive")),
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub struct SymbolTable<'a> {
//     symbols: HashMap<String, Expr<'a>>,
// }

// #[derive(Debug, Clone)]
// pub struct Block<'a> {
//     label: usize,
//     symbols: SymbolTable<'a>,
//     expr: Option<Box<Expr<'a>>>,
//     nested_in: Option<usize>,
// }

// #[derive(Debug, Clone)]
// pub enum BinaryOps {
//     Plus,
// }

// impl TryFrom<LexedResultType<'_>> for BinaryOps {
//     type Error = String;

//     fn try_from(value: LexedResultType) -> Result<Self, Self::Error> {
//         match value {
//             LexedResultType::Plus => Ok(BinaryOps::Plus),
//             _ => Err(String::from("Unexpected operator...")),
//         }
//     }
// }

// #[derive(Debug, Clone)]
// pub enum Expr<'a> {
//     Definitions(Definitions<'a>),
//     Application {
//         /// When [function] is None, expect that
//         /// the function we are applying is that of
//         /// a lambda
//         function: Option<String>,
//         arguments: Vec<Box<Expr<'a>>>,
//     },
//     Type(Types),
//     Block(Block<'a>),
//     Reference {
//         name: String,
//         ty: Types,
//         block: usize,
//     },
//     BinaryOp {
//         binder: BinaryOps,
//         lhs: Box<Expr<'a>>,
//         rhs: Box<Expr<'a>>,
//     },
//     Dud,
// }

// type ParseFnResult<'a> = Result<Expr<'a>, String>;

// #[derive(Debug)]
// pub struct Parser<'a> {
//     pub cursor: Cursor<LexedResult<'a>>,
//     pub symbols: SymbolTable<'a>,
//     pub block_inc_c: usize,
//     pub blocks: HashMap<usize, Block<'a>>,
//     pub current_block: Option<usize>,
// }

// // This is just nicer for me, but, for APIs in the future
// // might be better to follow the .init() style.
// impl<'a> From<Lexer<'a>> for Parser<'a> {
//     fn from(value: Lexer<'a>) -> Parser<'a> {
//         let mut symbols = HashMap::new();
//         symbols.insert(
//             "Str".to_string(),
//             Expr::Type(Types::Primitive(PrimitiveTypes::Str)),
//         );
//         symbols.insert(
//             "Int".to_string(),
//             Expr::Type(Types::Primitive(PrimitiveTypes::Int)),
//         );
//         let mut print_args = HashMap::new();
//         print_args.insert(
//             String::from("to_print"),
//             Types::Primitive(PrimitiveTypes::Str),
//         );
//         symbols.insert(
//             "print".to_string(),
//             Expr::Definitions(Definitions::Function {
//                 name: String::from("print"),
//                 arg_ty_list: vec![FnArg::Unnamed(Types::Primitive(PrimitiveTypes::Str))],
//                 return_ty: None,
//                 body: 0,
//                 phantom: PhantomData,
//             }),
//         );
//         Parser {
//             symbols: SymbolTable { symbols },
//             blocks: HashMap::new(),
//             block_inc_c: 0,
//             current_block: None,
//             cursor: Cursor::init(value.results),
//         }
//     }
// }

// pub enum AutoNestSetting {
//     AutoNestCurrent,
//     NestUnder(usize),
// }

// #[derive(Debug, Clone)]
// pub enum FnArg {
//     Unnamed(Types),
//     Named { name: String, ty: Types },
// }

// impl<'a> Parser<'a> {
//     pub fn expect(&mut self, expects: Vec<LexedResultType>) -> Result<bool, String> {
//         let expects_len = expects.len();
//         let Some(next_n) = self
//             .cursor
//             .source
//             .get(self.cursor.at..self.cursor.at + expects_len)
//         else {
//             return Err(format!(
//                 "Failed to get next N (dbg: prev={:?}, current={:?})",
//                 self.cursor.prev(),
//                 self.cursor.current()
//             ));
//         };
//         return Ok(next_n
//             .iter()
//             .map(|e| (e.to_owned()).ty)
//             .filter(|e| expects.contains(e))
//             .collect::<Vec<LexedResultType>>()
//             == expects);
//     }

//     pub fn expect_and_advance(&mut self, expects: Vec<LexedResultType>) -> Result<bool, String> {
//         let expects_len = expects.len();
//         let expect = self.expect(expects)?;
//         if expect {
//             self.cursor.advance(expects_len)?;
//         }
//         return Ok(expect);
//     }

//     pub fn current_block_mut(&mut self) -> Result<&mut Block<'a>, String> {
//         let Some(current_block) = self.current_block else {
//             return Err(String::from("No current block"));
//         };
//         return Ok(self.get_block(current_block).unwrap());
//     }

//     pub fn get_block(&mut self, block_id: usize) -> Option<&mut Block<'a>> {
//         if let Some(current_block) = self.current_block
//             && current_block == block_id
//         {
//             return self.blocks.get_mut(&current_block);
//         }
//         return self.blocks.get_mut(&block_id);
//     }

//     // function defaults to pushing current unless specified otherwise.
//     // [nest]: Defauls to nesting under current_block
//     pub fn push_new_block(
//         &mut self,
//         expr: Option<Expr<'a>>,
//         set_current: bool,
//         nest: Option<AutoNestSetting>,
//         symbol_table: Option<SymbolTable<'a>>,
//     ) -> Result<(), String> {
//         let block_name = self.block_inc_c;
//         self.block_inc_c += 1;
//         let symbols = symbol_table.unwrap_or(SymbolTable {
//             symbols: HashMap::new(),
//         });
//         let new_block = Block {
//             label: block_name,
//             symbols,
//             expr: expr.map(Box::new),
//             nested_in: match nest {
//                 Some(AutoNestSetting::NestUnder(under)) => Some(under),
//                 _ => None,
//             },
//         };
//         if set_current {
//             self.current_block = Some(new_block.label);
//         }
//         self.blocks.insert(new_block.label, new_block);
//         Ok(())
//     }

//     // This function will attempt to get the symbol from the CURRENT BLOCK
//     // or default to the global symbol table where function defs, and such,
//     // live.
//     pub fn get_symbol(
//         &mut self,
//         symbol: String,
//         look_in_block: Option<usize>,
//     ) -> ParseFnResult<'a> {
//         if let Some(look_in) = look_in_block
//             && let Some(block) = self.get_block(look_in)
//             && let Some(symbol) = block.symbols.symbols.get(symbol.as_str())
//         {
//             return Ok((*symbol).to_owned());
//         }
//         if let Ok(cb) = self.current_block_mut() {
//             if let Some(symbol) = cb.symbols.symbols.get(symbol.as_str()) {
//                 return Ok((*symbol).to_owned());
//             } else if let Some(nested_in) = cb.nested_in {
//                 dbg!(nested_in);
//                 return self.get_symbol(symbol, Some(nested_in));
//             }
//         }
//         if let Some(symbol) = self.symbols.symbols.get(symbol.as_str()) {
//             return Ok((*symbol).to_owned());
//         }

//         Err(String::from("Could not get symbol... like at all"))
//     }

//     /// Expects to start immediatly after the ::
//     pub fn parse_arg_list(
//         &mut self,
//         symbols: &mut SymbolTable<'a>,
//         end_at: LexedResultType<'a>,
//     ) -> Result<Vec<FnArg>, String> {
//         let mut args = Vec::new();
//         while let Ok(peeked) = self.cursor.current() {
//             let peeked: LexedResult<'a> = (*peeked).to_owned();
//             if self.expect_and_advance(vec![LexedResultType::Dash, LexedResultType::GreaterThan])? {
//                 continue;
//             } else if peeked.ty == end_at {
//                 self.cursor.advance(1)?;
//                 return Ok(args);
//             }
//             if let Ok(ty) = self.parse_ty() {
//                 self.cursor.advance(1)?;
//                 args.push(FnArg::Unnamed(ty));
//             } else if let LexedResultType::IdentLiteral(ident) = peeked.ty {
//                 let ident = ident.to_string();
//                 self.cursor.advance(1)?;
//                 let Ok(r#type) = &self.parse_ty() else {
//                     panic!(
//                         "Expected types in the function argument list. {:?}",
//                         self.cursor.current()
//                     );
//                 };

//                 symbols
//                     .symbols
//                     .insert(ident.to_owned(), Expr::Type(r#type.to_owned()));
//                 args.push(FnArg::Named {
//                     name: ident,
//                     ty: r#type.to_owned(),
//                 });
//             }
//         }

//         Ok(args)
//     }

//     pub fn parse_fn_application(&mut self, fn_def: Expr<'a>) -> ParseFnResult<'a> {
//         if LexedResultType::OpenP == self.cursor.current()?.ty {
//             println!("Open p");
//             self.cursor.advance(1)?;
//         }
//         let (args, opt_name) = match fn_def {
//             Expr::Definitions(Definitions::Function {
//                 name, arg_ty_list, ..
//             }) => (arg_ty_list, Some(name)),
//             Expr::Type(Types::FunctionType { arg_ty_list, .. }) => (
//                 arg_ty_list
//                     .into_iter()
//                     .map(|e| FnArg::Unnamed(*e))
//                     .collect::<Vec<FnArg>>(),
//                 None,
//             ),
//             _ => return Err(String::from("Passed incorrect expression")),
//         };
//         println!(">> {:?} (c:{:?})", opt_name, self.cursor.current());

//         let mut arc_c = args.len();
//         let mut arguments = Vec::new();
//         //  self.cursor.advance(1)?;
//         while let Ok(peeked) = self.cursor.current() {
//             println!("||> {:?}", peeked.ty);
//             if peeked.ty == LexedResultType::CloseP || arc_c == 0 {
//                 break;
//             }
//             arguments.push(Box::new(self.parse_expr()?));
//             self.cursor.advance(1)?;
//             arc_c -= 1;
//         }
//         println!(
//             "Arguemtns for: {:?} => (c:{:?}) {:?}",
//             opt_name,
//             arc_c,
//             arguments.to_owned()
//         );

//         self.cursor.advance(1)?;

//         if LexedResultType::CloseP == self.cursor.current()?.ty {
//             println!("close p?");
//             self.cursor.advance(1)?;
//         }

//         return Ok(Expr::Application {
//             function: opt_name,
//             arguments,
//         });
//     }

//     pub fn parse_fn_definition(&mut self, ident: String) -> ParseFnResult<'a> {
//         let mut symbol_table = SymbolTable {
//             symbols: HashMap::new(),
//         };
//         let lhs: Vec<FnArg> = self.parse_arg_list(&mut symbol_table, LexedResultType::Eq)?;
//         dbg!(&symbol_table);
//         self.push_new_block(None, true, None, Some(symbol_table))?;

//         println!("is it above parse expr?? {:?}", ident);
//         let rhs = self.parse_expr()?;
//         self.current_block_mut()?.expr = Some(Box::new(rhs.to_owned()));
//         return Ok(Expr::Definitions(Definitions::Function {
//             name: ident.to_owned(),
//             arg_ty_list: lhs,
//             return_ty: None,
//             body: self.current_block_mut()?.label,
//             phantom: PhantomData,
//         }));
//     }

//     pub fn parse_ty(&mut self) -> Result<Types, String> {
//         let current = self.cursor.current()?;
//         match &current.ty {
//             LexedResultType::IdentLiteral(ident) => {
//                 let ident: String = ident.to_string();
//                 if let Expr::Type(r#type) = self.get_symbol(ident.to_owned(), None)? {
//                     self.cursor.advance(1)?;
//                     return Ok(r#type);
//                 }
//                 todo!("prev {:?}", self.cursor.prev());
//             }
//             LexedResultType::OpenP => {
//                 self.cursor.advance(1)?;
//                 if let LexedResultType::CloseP = self.cursor.current()?.ty {
//                     return Ok(Types::PlaceholderTuple);
//                 } else {
//                     // this table is a useless allocation. ill refactor
//                     // this out when i rewrite the parse_arg_list fn
//                     let mut symbol_table = SymbolTable {
//                         symbols: HashMap::new(),
//                     };
//                     let arg_list = self
//                         .parse_arg_list(&mut symbol_table, LexedResultType::CloseP)?
//                         .into_iter()
//                         .map(|e| {
//                             Box::new(match e {
//                                 FnArg::Unnamed(ty) => ty,
//                                 FnArg::Named { ty, .. } => ty,
//                             })
//                         })
//                         .collect::<Vec<Box<Types>>>();

//                     return Ok(Types::FunctionType {
//                         arg_ty_list: arg_list,
//                         return_ty: None,
//                     });
//                 }
//             }
//             a @ _ => panic!("{:?}", a),
//         }
//     }

//     pub fn parse_expr(&mut self) -> ParseFnResult<'a> {
//         let current = self.cursor.current()?.to_owned();

//         let expr = match &current.ty {
//             // Definitions, assignments
//             LexedResultType::IdentLiteral(ident) => {
//                 let ident: std::borrow::Cow<str> = ident.to_owned();
//                 println!("Parsing = {:?}", ident);
//                 self.cursor.advance(1)?;

//                 let lhs = if self
//                     .expect_and_advance(vec![LexedResultType::Colon, LexedResultType::Colon])?
//                 {
//                     let ident_str = ident.to_string();
//                     let definition = self.parse_fn_definition(ident_str.to_owned())?;
//                     self.symbols
//                         .symbols
//                         .insert(ident_str.to_owned(), definition.to_owned());

//                     definition
//                 } else if let Ok(symbol) = self.get_symbol(ident.to_string(), None) {
//                     match symbol {
//                         a @ Expr::Definitions(Definitions::Function { .. })
//                         | a @ Expr::Type(Types::FunctionType { .. }) => {
//                             self.parse_fn_application(a.to_owned())?
//                         }
//                         Expr::Type(ty) if let Some(cb) = self.current_block => Expr::Reference {
//                             name: ident.to_string(),
//                             ty,
//                             block: cb,
//                         },
//                         _ => symbol.to_owned(),
//                     }
//                 } else {
//                     panic!(
//                         "Unahndled circumstances {:?}\n\n{:#?}",
//                         ident.to_string(),
//                         self.get_symbol(ident.to_string(), None)
//                     )
//                 };
//                 lhs
//             }
//             ref a => {
//                 todo!("{a:?} (prev {:?})", self.cursor.prev())
//             }
//         };

//         if let Some(operator) = match &self.cursor.peek()?.ty {
//             a @ LexedResultType::Plus => Some(a),
//             _ => None,
//         } {
//             let op: BinaryOps = (*operator).to_owned().try_into()?;
//             self.cursor.advance(1)?;

//             let rhs = Box::new(self.parse_expr()?);
//             return Ok(Expr::BinaryOp {
//                 binder: op,
//                 lhs: Box::new(expr),
//                 rhs,
//             });
//         }

//         return Ok(expr);
//     }
// }
