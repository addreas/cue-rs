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

    (match) => { RelOp::Match };
    (notmatch) => { RelOp::NotMatch };
}

#[macro_export]
macro_rules! match_basic {
    (@val $construct:ident, typ) => { $construct(Basic::Type) };
    (@val $construct:ident, bot) => { Value::Bottom };
    (@val $construct:ident, $a:ident) => { $construct(Basic::Value($a.clone())) };
    (@val $construct:ident, $op:tt $a:ident) => { $construct(Basic::Relation(crate::rel_op!($op), $a.clone())) };

    ($ainput:expr, $binput:expr, $construct:ident, $fallback:path, {
        $(
            (
                ($opa:tt $a:tt)
                $_:tt
                ($opb:tt $b:tt)
            )
            =>
            ( $($res:tt)+ ),
        )+
    }) => {
        match ($ainput, $binput) {
            $(
                ((crate::rel_op!($opa), $a), (crate::rel_op!($opb), $b)) => { match_basic!(@val $construct, $($res)+) }
                ((crate::rel_op!($opb), $b), (crate::rel_op!($opa), $a)) => { match_basic!(@val $construct, $($res)+) }
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

    ( $(($($a:tt)+))&+ ) => { Value::Conjunction(vec![$( crate::cue_val!($($a)+).into() ),+ ]) };
    ( $(($($a:tt)+))|+ ) => { Value::Disjunction(vec![$( crate::cue_val!($($a)+).into() ),+ ]) };

    ({ $($k:ident: ($($v:tt)+)),* }) => {
        crate::adt::value::Value::Struct(vec![
            $(crate::adt::value::Field {
                label: stringify!($k).into(),
                optional: None,
                definition: false,
                hidden: false,
                value: crate::cue_val!($($v)+).into(),
            }),*
        ])
    };

    ([ $( ($($v:tt)+) ),* ]) => {
        Value::List(vec![
            $( crate::cue_val!($($v)+).into() ),*
        ])
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
