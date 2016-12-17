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

pub fn is_numeric_val(t: &Term) -> bool {
    match *t {
        Term::Zero => true,
        Term::Succ(ref t) => is_numeric_val(t),
        _ => false,
    }
}

pub fn eval1(t: &Term) -> Option<Term> {
    match *t {
        /// True-valued if statement
        Term::If(box Term::True, ref t2, _) => Some(*t2.clone()),
        /// False-valued if statement
        Term::If(box Term::False, _, ref t3) => Some(*t3.clone()),
        /// Evaluate the condition first, then return an if statement
        /// that has the condition evaluated to a true or false
        Term::If(ref cond, ref t2, ref t3) => {
            eval1(cond).map(|boolean| Term::If(box boolean, box *t2.clone(), box *t3.clone()))
        }
        /// Succ(t) means we have to evaluate t first to simplify it, then we
        /// rewrite the term to Succ(evaled)
        Term::Succ(ref t) => eval1(t).map(|evaled| Term::Succ(box evaled)),
        /// The predecessor of Zero is defined as Zero
        Term::Pred(box Term::Zero) => Some(Term::Zero),
        /// The predecessor of the successor of a value is the value itself (if it was a numeric
        /// value to begin with)
        Term::Pred(box Term::Succ(ref nv1)) if is_numeric_val(nv1) => Some(*nv1.clone()),
        /// Pred(t) means we have to evaluate t first to simplify it, then we
        /// rewrite the term to Pred(evaled)
        Term::Pred(ref t) => eval1(t).map(|evaled| Term::Pred(box evaled)),
        /// By definition, the Zero term IsZero.
        Term::IsZero(box Term::Zero) => Some(Term::True),
        /// If the term is numeric and the successor of a value, then it cannot be IsZero
        Term::IsZero(box Term::Succ(ref nv1)) if is_numeric_val(nv1) => Some(Term::False),
        /// If a term evals to Zero, then the term IsZero
        Term::IsZero(ref t) => eval1(t).map(|evaled| Term::IsZero(box evaled)),
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
