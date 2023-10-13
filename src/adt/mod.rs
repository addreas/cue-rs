pub mod op;
pub mod value;
pub mod eval;

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

    (_) => { crate::adt::value::Value::Top };
    (_|_) => { crate::adt::value::Value::Bottom };

    (null) => { crate::adt::value::Value::Null };
    (bool) => { crate::adt::value::Value::Bool(None) };
    (int) => { crate::adt::value::Value::Int(crate::adt::value::Basic::Type) };
    (float) => { crate::adt::value::Value::Float(crate::adt::value::Basic::Type) };
    (bytes) => { crate::adt::value::Value::Bytes(crate::adt::value::Basic::Type) };
    (string) => { crate::adt::value::Value::String(crate::adt::value::Basic::Type) };

    ($a:literal) => { crate::adt::value::Value::from($a) };

    ($op:tt $a:literal) => { crate::adt::value::Value::from((crate::rel_op!($op), $a)) };
    ($op:tt~ $a:literal) => { crate::adt::value::Value::from((crate::rel_op!($op~), $a)) };

    ( $(($($a:tt)+))&+ ) => { crate::adt::value::Value::Conjunction([$( crate::cue_val!($($a)+).into() ),+ ].into()) };
    ( $(($($a:tt)+))|+ ) => { crate::adt::value::Value::Disjunction([$( crate::cue_val!($($a)+).into() ),+ ].into()) };

    (@label [ $($val:tt)+ ]) => { crate::adt::value::Label::Bulk(crate::cue_val!( $($val)+ ).into()) };
    (@label $k:ident) => { crate::adt::value::Label::Single(stringify!($k).into(), None, None) };
    (@label $k:literal) => { crate::adt::value::Label::Single($k.into(), None, None) };
    ({ $( ($($k:tt)+) : ($($v:tt)+)),* }) => {
        crate::adt::value::Value::Struct([
            $(crate::adt::value::Field {
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
