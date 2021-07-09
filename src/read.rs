use crate::core::{Obj, Object, Interp};
use crate::object;
#[macro_use]
use crate::{pair, int, sym};

use std::iter::{self, Iterator};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum EofContext {
    InSym,
    InList,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ReadError {
    UnexpectedEof(EofContext),
    Eof,  // for when nothing is expected anyway
}

// Read doesn't take ownership of the iterator, thus this hack.
// You can stack these, but... please, please don't.
pub struct PushBack<'a, C> {
    pushed: Option<char>,
    rest: &'a mut C,
}

impl<'a, C> PushBack<'a, C> {
    pub fn new(pushed: char, rest: &'a mut C) -> Self {
        Self {
            pushed: Some(pushed),
            rest,
        }
    }
}

impl<'a, C: Iterator<Item=char>> Iterator for PushBack<'a, C> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(c) = self.pushed {
            self.pushed = None;
            Some(c)
        } else {
            self.rest.next()
        }
    }
}

fn read<C>(interp: &mut Interp, source: &mut C) -> Result<Obj, ReadError>
    where
        C: Iterator<Item=char>
{
    'next: loop {
        match source.next() {
            None => return Err(ReadError::Eof),
            Some(c) => match c {
                c if c.is_whitespace() => continue,
                ';' => 'comment: loop {
                    match source.next() {
                        None => return Err(ReadError::Eof),
                        Some('\n') => continue 'next,
                        Some(_) => (),
                    }
                },
                '"' => {
                    let mut buffer = String::new();
                    'sym: loop {
                        match source.next() {
                            None => return Err(ReadError::UnexpectedEof(EofContext::InSym)),
                            Some('"') => return Ok(interp.object(Object::Sym(buffer))),
                            Some(c) => buffer.push(c),
                        }
                    }
                },
                '(' => {
                    let mut list: Obj = None;
                    'pair: loop {
                        match source.next() {
                            None => return Err(ReadError::UnexpectedEof(EofContext::InList)),
                            Some(c) if c.is_whitespace() => continue 'pair,
                            Some(')') => return Ok(object::list_reverse(interp, &list, false)),
                            Some('.') => {
                                let tail = read(interp, source)
                                    .map_err(|e| match e {
                                        ReadError::Eof => ReadError::UnexpectedEof(EofContext::InList),
                                        x => x,
                                    })?;
                                let chr = 'skipws: loop {
                                    match source.next() {
                                        None => return Err(ReadError::UnexpectedEof(EofContext::InList)),
                                        Some(c) if c.is_whitespace() => continue 'skipws,
                                        Some(c) => break 'skipws c,
                                    }
                                };
                                if chr == ')' {
                                    list = pair!(interp, tail, list);
                                    return Ok(object::list_reverse(interp, &list, true));
                                } else {
                                    // We consumed one character of something, so now we have to
                                    // read it.
                                    let after = read(interp, &mut PushBack::new(chr, source))
                                        .map_err(|e| match e {
                                            ReadError::Eof => ReadError::UnexpectedEof(EofContext::InList),
                                            x => x,
                                        })?;
                                    list = pair!(interp, sym!(interp, "."), tail, after, list);
                                    continue 'pair;
                                }
                                // statically unreachable
                            },
                            Some(c) => {
                                let item = read(interp, &mut PushBack::new(c, source))
                                    .map_err(|e| match e {
                                        ReadError::Eof => ReadError::UnexpectedEof(EofContext::InList),
                                        x => x,
                                    })?;
                                list = pair!(interp, item, list);
                                continue 'pair;
                            },
                        }
                    }
                },
                c => {
                    let mut buffer = String::new();
                    buffer.push(c);
                    'sym: loop {
                        match source.next() {
                            None => break 'sym,
                            Some(c) if c.is_whitespace() => break 'sym,
                            Some(c) => buffer.push(c),
                        }
                    }
                    if buffer.chars().all(|c| c.is_digit(10)) {
                        return Ok(int!(interp, buffer.parse::<isize>().unwrap()));
                    }
                    return Ok(interp.object(Object::Sym(buffer)));
                }
            },
        }
    }
    // statically unreachable
}
