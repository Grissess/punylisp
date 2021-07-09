use crate::{Gc, Trace, Arena, Visitor};

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

    pub fn push_value(&mut self, val: Obj) {
        self.state.values = Some(self.arena.borrow_mut().gc(
            Object::Pair(val, self.state.values.clone())
        ));
    }

    pub fn pop_value(&mut self) -> Obj {
        let popped = self.state.values.clone();
        self.state.values = self.state.values.as_ref().and_then(|o| match o.as_ref() {
            Some(Object::Pair(_, next)) => next.clone(),
            Some(_) => Some(o.clone()),
            None => unreachable!(),
        });
        popped
    }

    pub fn r#true(&self) -> Obj { Some(self.true_.clone()) }
    pub fn r#false(&self) -> Obj { Some(self.false_.clone()) }
}
