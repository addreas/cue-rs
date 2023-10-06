pub mod op;
pub mod value;
pub mod eval;

mod macros {
#[macro_export]
macro_rules! rel_op {
    (>) => { RelOp::GreaterThan };
    (>=) => { RelOp::GreaterEqual };
    (<) => { RelOp::LessThan };
    (<=) => { RelOp::LessEqual };
    (!=) => { RelOp::NotEqual };
    (=~) => { RelOp::Match };
    (!~) => { RelOp::NotMatch };
}

#[macro_export]
macro_rules! match_basic {
    (@val $construct:ident, _) => { $construct(Basic::Type) };
    (@val $construct:ident, _|_) => { Value::Bottom };
    (@val $construct:ident, $a:ident) => { $construct(Basic::Value($a.clone())) };
    (@val $construct:ident, $op:tt $a:ident) => { $construct(Basic::Relation(crate::rel_op!($op), $a.clone())) };
    (@val $construct:ident, $op:tt~ $a:ident) => { $construct(Basic::Relation(crate::rel_op!($op~), $a.clone())) };

    (@pat $op:tt $val:tt) => { (crate::rel_op!($op), $val) };
    (@pat $op:tt~ $val:tt) => { (crate::rel_op!($op~), $val) };

    ($ainput:expr, $binput:expr, $construct:ident, $fallback:path, {
        $(
            (
                ($($lhs:tt)+)
                $_:tt
                ($($rhs:tt)+)
            )
            =>
            ( $($res:tt)+ ),
        )+
    }) => {
        match ($ainput, $binput) {
            $(
                (match_basic!(@pat $($lhs)+), match_basic!(@pat $($rhs)+)) => { match_basic!(@val $construct, $($res)+) }
                (match_basic!(@pat $($rhs)+), match_basic!(@pat $($lhs)+)) => { match_basic!(@val $construct, $($res)+) }
            ),+
            (_, _) => Value::Bottom,
        }
    };
}

#[macro_export]
macro_rules! cue_val {
    (_) => { Value::Top };
    (_|_) => { Value::Bottom };

    (null) => { Value::Null };
    (bool) => { Value::Bool(None) };
    (int) => { Value::Int(Basic::Type) };
    (float) => { Value::Float(Basic::Type) };
    (bytes) => { Value::Bytes(Basic::Type) };
    (string) => { Value::String(Basic::Type) };

    ($a:literal) => { crate::adt::value::Value::from($a) };

    ($op:tt $a:literal) => { crate::adt::value::Value::from((crate::rel_op!($op), $a)) };
    ($op:tt~ $a:literal) => { crate::adt::value::Value::from((crate::rel_op!($op~), $a)) };

    ( $(($($a:tt)+))&+ ) => { Value::Conjunction([$( crate::cue_val!($($a)+).into() ),+ ].into()) };
    ( $(($($a:tt)+))|+ ) => { Value::Disjunction([$( crate::cue_val!($($a)+).into() ),+ ].into()) };

    ({ $($k:ident: ($($v:tt)+)),* }) => {
        crate::adt::value::Value::Struct([
            $(crate::adt::value::Field {
                label: crate::adt::value::Label {
                    name: stringify!($k).into(),
                    optional: None,
                    definition: false,
                    hidden: false,
                },
                value: crate::cue_val!($($v)+).into(),
            }),*
        ].into())
    };

    ([ $( ($($v:tt)+) ),* ]) => {
        Value::List([
            $( crate::cue_val!($($v)+).into() ),*
        ].into())
    };
}

#[macro_export]
macro_rules! assert_cue {
    (($($a:tt)+) & ($($b:tt)+) == ($($c:tt)+)) => {
        let a = Rc::from(crate::cue_val!($($a)+));
        let b = Rc::from(crate::cue_val!($($b)+));
        let c = Rc::from(crate::cue_val!($($c)+));
        assert_eq!(
            a.meet(b),
            c,
            "expect that ({}) & ({}) == ({})",
            stringify!($($a)+),
            stringify!($($b)+),
            stringify!($($c)+),
        )
    };

    (($($a:tt)+) | ($($b:tt)+) == ($($c:tt)+)) => {
        let a = Rc::from(crate::cue_val!($($a)+));
        let b = Rc::from(crate::cue_val!($($b)+));
        let c = Rc::from(crate::cue_val!($($c)+));
        assert_eq!(
            a.join(b),
            c,
            "expect that ({}) | ({}) == ({})",
            stringify!($($a)+),
            stringify!($($b)+),
            stringify!($($c)+),
        )
    };
}

}
