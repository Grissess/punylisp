extern crate tracing_gc as gc;

pub use gc::{Gc, Arena, Trace, Visitor};

pub mod core;
pub use crate::core::{Interp, Object, Obj};
pub mod object;
pub mod read;
pub mod builtin;

#[cfg(test)]
mod test;
