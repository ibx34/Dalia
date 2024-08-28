use std::{borrow::Cow, fmt::Debug, ops::Range, usize};

#[derive(Debug)]
pub struct Cursor<A: Debug + Clone + PartialEq> {
    pub source: Vec<A>,
    pub source_len: usize,
    pub at: usize,
    pub previous: A,
    pub line: usize,
}

impl<A: Debug + Clone + PartialEq> Cursor<A> {
    pub fn init(source: Vec<A>) -> Cursor<A> {
        let source_len = source.len();
        let first = source.get(0).unwrap();
        Cursor {
            previous: first.to_owned(),
            source,
            source_len,
            at: 0,
            line: 0,
        }
    }

    pub fn advance(&mut self, amt: usize) -> Result<(), String> {
        if amt != 0 {
            self.previous = self.current()?.to_owned();
        }
        self.at += amt;
        if self.at > self.source.len() {
            self.at -= amt;
            return Err(String::from("Reached EOF"));
        }
        Ok(())
    }

    pub fn advance_ret(&mut self, amt: usize) -> Result<&A, String> {
        self.advance(amt)?;
        return self.current();
    }

    pub fn newline(&mut self) {
        self.line += 1;
    }

    pub fn peek(&mut self) -> Result<&A, String> {
        if self.at + 1 > self.source_len {
            return Err(String::from("Reached EOF"));
        }
        if let Some(source_at) = self.source.get(self.at + 1) {
            return Ok(source_at);
        }
        Err(String::from("Failed to retrieve character from source."))
    }

    pub fn current(&mut self) -> Result<&A, String> {
        if let Some(source_at) = self.source.get(self.at) {
            return Ok(source_at);
        }
        Err(String::from("Failed to retrieve character from source."))
    }

    pub fn prev(&mut self) -> Result<A, String> {
        Ok(self.previous.to_owned())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum LexedResultType<'a> {
    Colon,
    Dash,
    GreaterThan,
    Newline,
    LessThan,
    Backslash,
    ForwardSlash,
    Character(char),
    Plus,
    Minus,
    Eq,
    OpenP,
    CloseP,
    CurlyOpenP,
    Space,
    CurlyCloseP,
    Bang,
    IdentLiteral(Cow<'a, str>),
    StrLiteral(Cow<'a, str>),
    NumLiteral(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexedResult<'a> {
    pub ty: LexedResultType<'a>,
    pub at: Range<usize>,
    pub line: usize,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    pub cursor: Cursor<char>,
    pub results: Vec<LexedResult<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn init(source: Vec<char>) -> Lexer<'a> {
        let cursor = Cursor::init(source);
        Lexer {
            cursor,
            results: Vec::new(),
        }
    }

    pub fn create_lexed_result(&mut self, ty: LexedResultType<'a>) -> LexedResult<'a> {
        LexedResult {
            ty,
            at: self.cursor.at..self.cursor.at,
            line: self.cursor.line,
        }
    }

    pub fn all(&mut self) -> Result<(), String> {
        while let Ok(peeked) = self.cursor.current() {
            let peeked = peeked.to_owned();
            let lexed = self.lex(&peeked)?;
            match lexed.ty {
                a @ LexedResultType::Space | a @ LexedResultType::Newline => {
                    if a == LexedResultType::Newline {
                        self.cursor.line += 1;
                    }
                    self.cursor.advance(1)?;
                }
                LexedResultType::IdentLiteral(_) => self.results.push(lexed),
                _ => {
                    self.results.push(lexed);
                    self.cursor.advance(1)?;
                }
            }
        }
        Ok(())
    }

    pub fn lex(&mut self, char: &char) -> Result<LexedResult<'a>, String> {
        let res = match char {
            &'\n' => {
                self.cursor.newline();
                LexedResultType::Newline
            }
            &' ' => LexedResultType::Space,
            &':' => LexedResultType::Colon,
            &'!' => LexedResultType::Bang,
            &')' => LexedResultType::CloseP,
            &'(' => LexedResultType::OpenP,
            &'}' => LexedResultType::CurlyCloseP,
            &'{' => LexedResultType::CurlyOpenP,
            &'-' => LexedResultType::Dash,
            &'+' => LexedResultType::Plus,
            &'\\' => LexedResultType::Backslash,
            &'>' => LexedResultType::GreaterThan,
            &'<' => LexedResultType::LessThan,
            &'=' => LexedResultType::Eq,
            a => {
                if a.is_numeric() {
                    let starting_at = self.cursor.at;
                    let mut make_str: String = self.cursor.current()?.to_string();
                    self.cursor.advance(1)?;
                    while let Ok(char) = self.cursor.current() {
                        if !char.is_numeric() {
                            break;
                        }
                        make_str.push(char.to_owned());
                        self.cursor.advance(1)?;
                    }
                    let int = make_str.parse::<usize>().unwrap();
                    return Ok(LexedResult {
                        ty: LexedResultType::NumLiteral(int),
                        at: starting_at..self.cursor.at,
                        line: self.cursor.line,
                    });
                }

                let is_str = a == &'"';
                let stop_at = match a {
                    &'"' => {
                        self.cursor.advance(1)?;
                        vec!['"']
                    }
                    a if ('a'..='Z').contains(a) ||  a != &'_' => {
                        vec![' ', '\n']
                    }
                    _ => return Err(String::from("Did not match any character and did not qualify for special treatment (\" or alphabetic)"))
                };
                let mut make_str: String = self.cursor.current()?.to_string();
                let starting_at = self.cursor.at;
                self.cursor.advance(1)?;

                while let Ok(char) = self.cursor.current() {
                    if stop_at.contains(char) || (!is_str && !char.is_alphabetic() && char != &'_')
                    {
                        break;
                    }
                    make_str.push(char.to_owned());
                    self.cursor.advance(1)?;
                }

                return Ok(LexedResult {
                    ty: match is_str {
                        true => LexedResultType::StrLiteral(Cow::Owned(make_str)),
                        false => LexedResultType::IdentLiteral(Cow::Owned(make_str)),
                    },
                    at: starting_at..self.cursor.at,
                    line: self.cursor.line,
                });
            }
        };
        return Ok(self.create_lexed_result(res));
    }
}
