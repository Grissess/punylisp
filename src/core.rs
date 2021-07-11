use crate::{Gc, Trace, Arena, Visitor};
use crate::object::first;
#[macro_use]
use crate::pair;

use std::cell::RefCell;

pub type Obj = Option<Gc<Object>>;

pub type BuiltinFn = fn(
    interp: &mut Interp, 
    args: Obj,
    state: Obj,
);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PassBy {
    Name,
    Value,
}

pub enum FuncCode {
    User { body: Obj, },
    Builtin { func: BuiltinFn, },
}

pub struct Callable {
    pass_by: PassBy,
    code: FuncCode,
}

pub enum Object {
    Int(isize),
    Sym(String),
    Pair(Obj, Obj),
    Func(Callable),
    Cont(InterpState),
}

#[derive(Clone)]
pub struct InterpState {
    values: Obj,
    continuations: Obj,
    environ: Obj,
}

#[derive(Debug, Clone, Copy)]
pub struct GcState {
    events_now: usize,
    events_collect: Option<usize>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Value {
    Direct,
    Syntactic,
}

pub struct Interp {
    arena: RefCell<Arena>,
    state: InterpState,
    init_env: Obj,
    pub error: Obj,
    true_: Gc<Object>,
    false_: Gc<Object>,
    gc_state: GcState,
}

impl Trace for Object {
    fn trace(&self, visitor: &Visitor) {
        use Object::*;

        match self {
            Int(_) | Sym(_) => (),
            Pair(first, next) => {
                if let Some(o) = first {
                    visitor.visit(&o);
                }
                if let Some(o) = next {
                    visitor.visit(&o);
                }
            },
            Func(Callable { code, .. }) => match code {
                FuncCode::User { body } => {
                    if let Some(o) = body {
                        visitor.visit(&o);
                    }
                },
                FuncCode::Builtin { .. } => (),
            },
            Cont(InterpState { values, continuations, environ }) => {
                if let Some(o) = values {
                    visitor.visit(&o);
                }
                if let Some(o) = continuations {
                    visitor.visit(&o);
                }
                if let Some(o) = environ {
                    visitor.visit(&o);
                }
            },
        }
    }
}

impl Interp {
    // Interning an object is logically mutation, but it's inconvenient to assert the mutable
    // borrow because, often, these come about as nested temporaries. Since Arena::gc is logically
    // just a move anyway, this probably isn't too unsafe.
    pub fn object(&self, val: Object) -> Obj {
        Some(self.arena.borrow_mut().gc(val))
    }

    pub fn push_value(&mut self, val: Obj, ds: Value) {
        let flag = if ds == Value::Syntactic {
            self.r#true()
        } else {
            self.r#false()
        };
        self.state.values = pair!(
            self,
            pair!(self, val, flag),
            self.state.values.clone()
        );
    }

    pub fn pop_value(&mut self) -> (Obj, Value) {
        let popped = first(&self.state.values).clone();
        match self.state.values.as_deref() {
            Some(Object::Pair(_, next)) => self.state.values = next.clone(),
            Some(_) => (),
            None => panic!("Pop from empty stack"),
        };
        if let Some(Object::Pair(val, flag)) = popped.as_deref() {
            (
                // FIXME: moving this--*val--should be fine, since popped is about to drop
                val.clone(),
                if self.is_true(flag) {
                    Value::Syntactic
                } else {
                    Value::Direct
                }
            )
        } else {
            panic!("Unclassified object on the value stack")
        }
    }

    pub fn r#true(&self) -> Obj { Some(self.true_.clone()) }
    pub fn r#false(&self) -> Obj { Some(self.false_.clone()) }

    pub fn is_true(&self, o: &Obj) -> bool {
        if let Some(gc) = o {
            Gc::ptr_eq(gc, &self.true_)
        } else {
            false
        }
    }

    pub fn is_false(&self, o: &Obj) -> bool {
        if let Some(gc) = o {
            Gc::ptr_eq(gc, &self.false_)
        } else {
            false
        }
    }

    pub fn is_truthy(&self, o: &Obj) -> bool {
        match o {
            None => false,
            Some(gc) => {
                if Gc::ptr_eq(gc, &self.false_) {
                    false
                } else {
                    match &**gc {
                        Object::Int(i) => *i != 0,
                        Object::Sym(s) => s.len() > 0,
                        _ => true,
                    }
                }
            }
        }
    }
}
