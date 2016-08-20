#![feature(box_syntax, box_patterns)]

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    True,
    False,
    Zero,
    IsZero(Box<Term>),
    Succ(Box<Term>),
    Pred(Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

pub fn isnumericval(t: &Term) -> bool {
    match t {
        &Term::Zero => true,
        &Term::Succ(ref t) => isnumericval(t),
        _ => false,
    }
}

pub fn isvalue(t: &Term) -> bool {
    match t {
        &Term::True => true,
        &Term::False => true,
        t => isnumericval(t),
    }
}

pub fn eval1(t: &Term) -> Option<Term> {
    match t {
        &Term::If(box Term::True, ref t2, _) => Some(*t2.clone()),
        &Term::If(box Term::False, _, ref t3) => Some(*t3.clone()),
        &Term::If(ref fi, ref t2, ref t3) => {
            eval1(fi).map(|t| Term::If(box t, box *t2.clone(), box *t3.clone()))
        }
        &Term::Succ(ref t) => eval1(t).map(|t| Term::Succ(box t)),
        &Term::Pred(box Term::Zero) => Some(Term::Zero),
        &Term::Pred(box Term::Succ(ref nv1)) if isnumericval(nv1) => Some(*nv1.clone()),
        &Term::Pred(ref t) => eval1(t).map(|t| Term::Pred(box t)),
        &Term::IsZero(box Term::Zero) => Some(Term::True),
        &Term::IsZero(box Term::Succ(ref nv1)) if isnumericval(nv1) => Some(Term::False),
        &Term::IsZero(ref t) => eval1(t).map(|t| Term::IsZero(box t)),
        _ => None,
    }
}

pub fn eval(t: &Term) -> Option<Term> {
    Some(eval1(t)
             .and_then(|t| eval(&t))
             .unwrap_or(t.clone()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant_true() {
        assert_eq!(eval(&Term::True).unwrap(), Term::True);
    }

    #[test]
    fn constant_false() {
        assert_eq!(eval(&Term::False).unwrap(), Term::False);
    }

    #[test]
    fn constant_zero() {
        assert_eq!(eval(&Term::Zero).unwrap(), Term::Zero);
    }

    #[test]
    fn is_zero() {
        assert_eq!(eval(&Term::IsZero(box Term::Zero)).unwrap(), Term::True);
    }

    #[test]
    #[should_panic]
    fn is_zero_nonnumeric() {
        assert_eq!(eval(&Term::IsZero(box Term::False)).unwrap(), Term::False);
    }

    #[test]
    fn is_non_zero() {
        assert_eq!(eval(&Term::IsZero(box Term::Succ(box Term::Zero))).unwrap(),
                   Term::False);
    }

    #[test]
    fn pred_succ() {
        assert_eq!(eval(&Term::Pred(box Term::Succ(box Term::Zero))).unwrap(),
                   Term::Zero);
    }

    #[test]
    fn pred_succ_zero() {
        assert_eq!(eval(&Term::IsZero(box Term::Pred(box Term::Succ(box Term::Zero)))).unwrap(),
                   Term::True);
    }

    #[test]
    fn pred_succ_succ() {
        assert_eq!(eval(&Term::Pred(box Term::Succ(box Term::Succ(box Term::Zero)))).unwrap(),
                   Term::Succ(box Term::Zero));
    }

    #[test]
    fn if_is_zero() {
        assert_eq!(eval(&Term::If(box Term::IsZero(box Term::Zero),
                                  box Term::True,
                                  box Term::False))
                       .unwrap(),
                   Term::True);
    }

    #[test]
    fn if_is_non_zero() {
        assert_eq!(eval(&Term::If(box Term::IsZero(box Term::Succ(box Term::Zero)),
                                  box Term::Zero,
                                  box Term::Succ(box Term::Zero)))
                       .unwrap(),
                   Term::Succ(box Term::Zero));
    }
}
