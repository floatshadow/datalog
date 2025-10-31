///|
typealias @immut/hashmap.HashMap

///|
typealias @immut/hashset.HashSet

///|
typealias @list.List

///|
fn[K : Eq + Hash, V] HashMap::add_multi(
  self : HashMap[K, Array[V]],
  key : K,
  value : V,
) -> HashMap[K, Array[V]] {
  match self.get(key) {
    None => self.add(key, [value])
    Some(vs) => {
      vs.push(value)
      self
    }
  }
}

///|
fn[K : Eq + Hash, V : Eq + Hash] HashMap::insert_multi(
  self : HashMap[K, HashSet[V]],
  key : K,
  value : V,
) -> HashMap[K, HashSet[V]] {
  match self.get(key) {
    None => self.add(key, HashSet::new().add(value))
    Some(vs) => self.add(key, vs.add(value))
  }
}

///|
/// Monadic substitution operation
fn[K, V] subst_mapM(
  m : HashMap[K, V]?,
  f : (HashMap[K, V]) -> HashMap[K, V]? raise?,
) -> HashMap[K, V]? raise? {
  match m {
    None => None
    Some(m) => f(m)
  }
}

///|
pub enum Constant {
  IdentC(String)
  StringC(String)
  IntC(Int)
  BoolC(Bool)
} derive(Eq, Hash)

///|
pub enum Term {
  Var(String)
  Const(Constant)
} derive(Eq, Hash)

///|
pub struct Literal {
  predicate : String
  arity : Int
  terms : Array[Term]
} derive(Eq, Hash)

///|
pub struct Clause {
  head : Literal
  body : Array[Literal]
} derive(Eq, Hash)

///|
fn Constant::from(c : @syntax.Constant) -> Constant {
  match c {
    IdentC(id) => IdentC(id.inner)
    StringC(str) => StringC(str.inner)
    IntC(i) => IntC(i.inner)
    BoolC(b) => BoolC(b.inner)
  }
}

///|
fn Term::from(t : @syntax.Term) -> Term {
  match t {
    Var(name) => Var(name.inner)
    Const(c) => Const(Constant::from(c.inner))
  }
}

///|
fn Literal::from(lit : @syntax.Literal) -> Literal {
  match lit.terms {
    None => Literal::{ predicate: lit.predicate.inner, arity: 0, terms: [] }
    Some(tms) => {
      let tms = tms.to_array().map(fn(t) { Term::from(t.inner) })
      Literal::{
        predicate: lit.predicate.inner,
        arity: tms.length(),
        terms: tms,
      }
    }
  }
}

///|
/// Is the literal ground (no variables)?
fn Literal::is_fact(self : Literal) -> Bool {
  self.terms
  .iter()
  .all(fn(t) {
    match t {
      Const(_) => true
      Var(_) => false
    }
  })
}

///|
pub impl Show for Constant with output(self, logger) {
  match self {
    IdentC(id) => Show::output(id, logger)
    StringC(str) => {
      logger.write_char('\"')
      Show::output(str, logger)
      logger.write_char('\"')
    }
    IntC(i) => Show::output(i, logger)
    BoolC(b) => Show::output(b, logger)
  }
}

///|
pub impl Show for Term with output(self, logger) {
  match self {
    Var(name) => Show::output(name, logger)
    Const(c) => Show::output(c, logger)
  }
}

///|
pub impl Show for Literal with output(self, logger) {
  Show::output(self.predicate, logger)
  if self.arity > 0 {
    logger.write_char('(')
    for i = 0; i < self.terms.length(); {
      Show::output(self.terms[i], logger)
      if i < self.terms.length() - 1 {
        logger.write_string(", ")
      }
      continue i + 1
    }
    logger.write_char(')')
  }
}

///|
pub impl Show for Clause with output(self, logger) {
  Show::output(self.head, logger)
  if self.body.length() > 0 {
    logger.write_string(" :- ")
    for i = 0; i < self.body.length(); {
      Show::output(self.body[i], logger)
      if i < self.body.length() - 1 {
        logger.write_string(", ")
      }
      continue i + 1
    }
  }
  logger.write_char('.')
}

///|
typealias HashMap[String, Term] as Subst

///|
typealias HashMap[String, HashSet[Literal]] as Database

///|
typealias HashMap[String, Int] as ArityMap

///|
// fn Database::copy(self : Database) -> Database {
//   self.map_with_key(fn(_k, v) { v.copy() })
// }

///|
fn ArityMap::check(
  self : ArityMap,
  subgoals : Array[Literal],
) -> ArityMap raise Failure {
  let mut arity = self
  for lit in subgoals {
    let relation = lit.predicate
    match arity.get(relation) {
      None => arity = arity.add(relation, lit.arity)
      Some(n) =>
        if n != lit.arity {
          fail("\{lit} has arity \{lit.arity} but \{relation} has arity \{n}")
        }
    }
  }
  self
}

///|
fn Term::apply_subst(self : Term, subst : Subst) -> Term {
  match self {
    Const(c) => Const(c)
    Var(name) =>
      match subst.get(name) {
        None => Var(name)
        Some(c) => c
      }
  }
}

///|
/// Apply substitution to a literal.
fn Literal::apply_subst(self : Literal, subst : Subst) -> Literal {
  let terms = self.terms.map(fn(t) { t.apply_subst(subst) })
  Literal::{ ..self, terms, }
}

///|
fn Subst::unify_literal(
  self : Subst,
  lhs : Literal,
  rhs : Literal,
) -> Subst? raise Failure {
  let lhs = lhs.apply_subst(self)
  let rhs = rhs.apply_subst(self)
  if lhs.predicate != rhs.predicate {
    return None
  }
  guard lhs.arity == rhs.arity else {
    fail("\{lhs} and \{rhs} has the same relation but with different arity")
  }
  let lhs_terms = lhs.terms
  let rhs_terms = rhs.terms
  fn solve(subst : Subst, variable : String, term : Term) -> Subst? {
    Some(subst.add(variable, term))
  }

  fn unify_term(subst : Subst?, lhs : Term, rhs : Term) -> Subst? {
    match (lhs, rhs) {
      (Const(lc), Const(rc)) => if lc == rc { subst } else { None }
      (Var(name), _) => subst_mapM(subst, fn(s) { solve(s, name, rhs) })
      (_, Var(name)) => subst_mapM(subst, fn(s) { solve(s, name, lhs) })
    }
  }

  let mut subst = Some(self)
  for term_pairs in lhs_terms.zip(rhs_terms) {
    let (lhs_term, rhs_term) = term_pairs
    subst = unify_term(subst, lhs_term, rhs_term)
  }
  subst
}
