pub mod op;
pub mod eval;
pub mod scope;

mod macros {
#[macro_export]
macro_rules! rel_op {
    (>) => { crate::adt::op::RelOp::GreaterThan };
    (>=) => { crate::adt::op::RelOp::GreaterEqual };
    (<) => { crate::adt::op::RelOp::LessThan };
    (<=) => { crate::adt::op::RelOp::LessEqual };
    (!=) => { crate::adt::op::RelOp::NotEqual };
    (=~) => { crate::adt::op::RelOp::Match };
    (!~) => { crate::adt::op::RelOp::NotMatch };
}

#[macro_export]
macro_rules! match_basic {
    (@val $construct:ident, _) => { Value::Top };
    (@val $construct:ident, _|_) => { Value::Bottom };
    (@val $construct:ident, $a:ident) => { $construct($a) };
    (@val $construct:ident, $op:tt $a:ident) => { crate::value::Value::Bound(crate::rel_op!($op), $construct($a).into()) };
    (@val $construct:ident, $op:tt~ $a:ident) => { crate::value::Value::Bound(crate::rel_op!($op~), $construct($a).into()) };

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

    (_) => { crate::value::Value::Top };
    (_|_) => { crate::value::Value::Bottom };

    (null) => { crate::value::Value::Null };
    (bool) => { crate::value::Value::Bool(None) };
    (int) => { crate::value::Value::Int(None) };
    (float) => { crate::value::Value::Float(None) };
    (bytes) => { crate::value::Value::Bytes(None) };
    (string) => { crate::value::Value::String(None) };

    ($a:literal) => { crate::value::Value::from($a) };

    ($op:tt $a:literal) => { crate::value::Value::from((crate::rel_op!($op), $a)) };
    ($op:tt~ $a:literal) => { crate::value::Value::from((crate::rel_op!($op~), $a)) };

    ( $(($($a:tt)+))&+ ) => { crate::value::Value::Conjunction([$( crate::cue_val!($($a)+).into() ),+ ].into()) };
    ( $(($($a:tt)+))|+ ) => { crate::value::Value::Disjunction([$( crate::cue_val!($($a)+).into() ),+ ].into()) };

    (@label [ $($val:tt)+ ]) => { crate::value::Label::Bulk(crate::cue_val!( $($val)+ ).into()) };
    (@label $k:ident) => { crate::value::Label::Single(stringify!($k).into(), None, None) };
    (@label $k:literal) => { crate::value::Label::Single($k.into(), None, None) };
    ({ $( ($($k:tt)+) : ($($v:tt)+)),* }) => {
        crate::value::Value::Struct([
            $(crate::value::Field {
                label: crate::cue_val!(@label $($k)+),
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
