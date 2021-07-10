use crate::core::{Obj, Object, Interp};

use std::iter::Iterator;

#[macro_export]
macro_rules! sym {
    ($interp:expr, $( $args:tt )*) => {
        $crate::core::Interp::object($interp, $crate::core::Object::Sym(format!($( $args )*)))
    };
}

#[macro_export]
macro_rules! int {
    ($interp:expr, $ival:expr) => {
        $crate::core::Interp::object($interp, $crate::core::Object::Int($ival))
    };
}

#[macro_export]
macro_rules! pair {
    ($interp:expr, $first:expr, $next:expr) => {
        $crate::core::Interp::object($interp, $crate::core::Object::Pair($first, $next))
    };
    ($interp:expr, $first:expr, $next:expr, $( $beyond:expr ),+) => {
        $crate::core::Interp::object($interp, $crate::core::Object::Pair(
                $first,
                pair!($interp, $next, $( $beyond ),+)
        ))
    };
}

macro_rules! declare_type_test {
    (@declare $name:ident, $match:pat, $default:expr) => {
        pub fn $name(obj: &Obj) -> bool {
            match obj.as_deref() {
                Some($match) => true,
                None => $default,
                _ => false,
            }
        }
    };
    ($name:ident, $match:pat, $default:expr) => {
        declare_type_test!(@declare $name, $match, $default);
    };
    ($name:ident, $match:pat) => {
        declare_type_test!(@declare $name, $match, false);
    };
}

declare_type_test!(is_int, Object::Int(_));
declare_type_test!(is_sym, Object::Sym(_));
declare_type_test!(is_pair, Object::Pair(_, _), true); // None is the empty list
declare_type_test!(is_func, Object::Func(_));
declare_type_test!(is_cont, Object::Cont(_));

pub struct ListIter<'a> {
    current: &'a Obj,
}

pub struct ListItem<'a> {
    pub pair: &'a Obj,
    pub item: &'a Obj,
}

impl<'a> Iterator for ListIter<'a> {
    type Item = ListItem<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            None => None,
            cur@Some(_) => {
                let item = ListItem {
                    pair: cur,
                    item: first(cur),
                };
                self.current = next(cur);
                Some(item)
            }
        }
    }
}

pub fn list_iter<'a>(obj: &'a Obj) -> Option<ListIter<'a>> {
    if !is_pair(obj) {
        None
    } else {
        Some(ListIter { current: obj })
    }
}

pub fn list_len(obj: &Obj) -> Option<usize> {
    list_iter(obj).map(Iterator::count)
}

pub fn list_reverse(interp: &mut Interp, obj: &Obj, improper: bool) -> Obj {
    let (mut result, mut start) = if improper {
        (first(obj).clone(), next(obj))
    } else {
        (None, obj)
    };
    match list_iter(obj) {
        None => (),
        Some(iter) => for item in iter {
            result = pair!(interp, item.item.clone(), result);
        },
    }
    result
}

// TODO: is &None static?
pub fn first(obj: &Obj) -> &Obj {
    if let Some(Object::Pair(f, _)) = obj.as_deref() {
        f
    } else {
        &None
    }
}

pub fn next(obj: &Obj) -> &Obj {
    if let Some(Object::Pair(_, n)) = obj.as_deref() {
        n
    } else {
        &None
    }
}
