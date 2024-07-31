use crate::lexer::{Cursor, LexedResult, Lexer};

pub enum ASTExpressions {
    LetIn {},
    FnDefinition {},
    FnCall {}
}

pub struct Parser<'a> {
    pub cursor: Cursor<LexedResult<'a>>,
    pub results: Vec<ASTExpressions>
}

// This is just nicer for me, but, for APIs in the future
// might be better to follow the .init() style.
impl<'a> From<Lexer<'a>> for Parser<'a> {
    fn from(value: Lexer<'a>) -> Parser<'a> {
        Parser {
            results: Vec::new(),
            cursor: Cursor::init(value.results)
        }
    }
}