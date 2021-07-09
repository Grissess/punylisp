use std::convert::TryInto;

use crate::core::{PassBy, BuiltinFn};
use crate::object::{first, next};
#[macro_use]
use crate::{pair, sym, int};

#[repr(C)]
pub struct BuiltinDecl {
    pass_by: PassBy,
    name: &'static str,
    func: BuiltinFn,
}

// Credit to Veedrac:
// https://stackoverflow.com/questions/38088067/equivalent-of-func-or-function-in-rust
#[macro_export]
macro_rules! func_name {
    () => {{
        fn f() {}
        // Until std::any::type_name_of_val stabilizes:
        fn type_name_of_val<T>(_: &T) -> &'static str {
            ::std::any::type_name::<T>()
        }
        let name = type_name_of_val(&f);
        &name[..name.len() - 3]  // clip off "::f"
    }};
}

#[macro_export]
macro_rules! builtin {
    () => {};
    (@declare $name:ident, $interp:ident, $args:ident, $state:ident, $load_name:expr, $static_name:ident, $pass_by:expr, $code:block) => {
        #[allow(unused_variables)]
        fn $name(
            $interp: &mut $crate::Interp,
            $args: $crate::Obj,
            $state: $crate::Obj,
        ) $code

        #[link_section = "pl_builtins"]
        #[used]
        #[allow(non_upper_case_globals)]
        static $static_name: BuiltinDecl = BuiltinDecl {
            pass_by: $pass_by,
            name: $load_name,
            func: $name as $crate::core::BuiltinFn,
        };
    };
    (@pass_kind by_name) => { $crate::core::PassBy::Name };
    (@pass_kind by_value) => { $crate::core::PassBy::Value };
    ($pass_by:tt fn $name:ident($interp:ident, $args:ident, $state:ident) as $load_name:expr, $static_name:ident $code:block $($rest:tt)*) => {
        builtin!(@declare
                 $name,
                 $interp,
                 $args,
                 $state,
                 $load_name,
                 $static_name,
                 builtin!(@pass_kind $pass_by),
                 $code
        );
        builtin!($($rest)*);
    };
}

#[macro_export]
macro_rules! ret {
    ($interp:expr, $expr:expr) => {
        let expr = $expr;  // Split borrow, since expr will likely mut interp
        $crate::Interp::push_value($interp, expr);
        return;
    };
    ($interp:expr) => {
        ret!($interp, $interp.r#true());
    };
}

#[macro_export]
macro_rules! err {
    ($interp:expr, $error:expr) => {
        $interp.error = $error;
        return;
    };
    ($interp:expr) => {
        $interp.error.clone()
    };
}

#[macro_export]
macro_rules! arity {
    (@do_test $interp:expr, $args:expr, $test:pat, $amount:expr) => {
        if let Some(len) = $crate::object::list_len(&$args) {
            if let $test = len.cmp(&$amount) {
                ()
            } else {
                err!($interp, pair!($interp, sym!($interp, "{}: bad arity: {}", func_name!(), stringify!($test)), int!($interp, len as isize), int!($interp, $amount as isize)));
            }
        } else {
            // Can unwrap here: list_len considers None a valid (0-len) list
            err!($interp, sym!($interp, "wrong arg type: {:?}", ::std::mem::discriminant($args.as_ref().unwrap())));
        }
    };
    ($interp:expr, $args:expr, > $amount:expr) => {
        arity!(@do_test $interp, $args, ::std::cmp::Ordering::Greater, $amount);
    };
    ($interp:expr, $args:expr, >= $amount:expr) => {
        arity!(@do_test $interp, $args, ::std::cmp::Ordering::Greater, $amount.saturating_sub(1usize));
    };
    ($interp:expr, $args:expr, = $amount:expr) => {
        arity!(@do_test $interp, $args, ::std::cmp::Ordering::Equal, $amount);
    };
    ($interp:expr, $args:expr, <= $amount:expr) => {
        arity!(@do_test $interp, $args, ::std::cmp::Ordering::Less, $amount.saturating_add(1usize));
    };
    ($interp:expr, $args:expr, < $amount:expr) => {
        arity!(@do_test $interp, $args, ::std::cmp::Ordering::Less, $amount);
    };
}

builtin! {
    by_value fn error(interp, args, state) as "error", decl_error {
        if let Some(o) = args {
            err!(interp, Some(o));
            // rest is unreachable
        } else {
            ret!(interp, err!(interp));
        }
    }

    by_value fn cons(interp, args, state) as "cons", decl_cons {
        arity!(interp, args, = 2);
        ret!(interp, pair!(interp, first(&args).clone(), first(next(&args)).clone()));
    }
}

extern "C" {
    static __start_pl_builtins: u8;
    static __stop_pl_builtins: u8;
}

pub fn declared_builtins() -> &'static [BuiltinDecl] {
    unsafe {
        let origin = std::ptr::addr_of!(__start_pl_builtins) as *const BuiltinDecl;
        let end = std::ptr::addr_of!(__stop_pl_builtins) as *const BuiltinDecl;
        std::slice::from_raw_parts(
            origin,
            end.offset_from(origin).try_into().unwrap(),
        )
    }
}

