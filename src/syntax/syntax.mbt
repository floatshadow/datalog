// https://docs.racket-lang.org/datalog/datalog.html

///|
typealias @util.List1

///|
typealias @util.Range

///|
typealias @util.Ranged

///|
traitalias @util.HasRange

// A variable is a sequence of Unicode "Uppercase" and
// "Lowercase" letters, digits, and the underscore character. 
// A variable must begin with a Unicode "Uppercase" letter.

// An identifier is a sequence of printing characters that 
// does not contain any of the following characters: (, `, ', ), =, :, ., ~, ?,
// ", %, and space. An identifier must not begin with a Latin capital letter.
// Note that the characters that start punctuation are forbidden in identifiers, but the hyphen character is allowed.

// A string is a sequence of characters enclosed in double quotes. 
// Characters other than double quote, newline, and backslash may be directly
// included in a string. The remaining characters may be specified 
// using escape characters, \", \ , and \\ respectively.

///|
pub(all) enum Constant {
  IdentC(Ranged[String])
  StringC(Ranged[String])
  IntC(Ranged[Int])
  BoolC(Ranged[Bool])
} derive(Show)

///|
pub(all) enum Term {
  Var(Ranged[String])
  Const(Ranged[Constant])
} derive(Show)

///|
pub(all) struct Literal {
  predicate : Ranged[String]
  terms : List1[Ranged[Term]]?
} derive(Show)

///|
pub(all) struct Clause {
  head : Ranged[Literal]
  body : List1[Ranged[Literal]]?
} derive(Show)

///|
pub(all) enum Statement {
  AssertS(Ranged[Clause])
  QueryS(Ranged[Literal])
} derive(Show)

///|
pub impl HasRange for Constant with get_range(self : Constant) -> Range {
  match self {
    IdentC(ranged_str) => ranged_str.get_range()
    StringC(ranged_str) => ranged_str.get_range()
    IntC(ranged_int) => ranged_int.get_range()
    BoolC(ranged_bool) => ranged_bool.get_range()
  }
}

///|
pub impl HasRange for Term with get_range(self : Term) -> Range {
  match self {
    Var(ranged_str) => ranged_str.get_range()
    Const(ranged_const) => ranged_const.get_range()
  }
}

///|
pub impl HasRange for Literal with get_range(self : Literal) -> Range {
  let predicate_range = self.predicate.get_range()
  match self.terms {
    None => predicate_range
    Some(terms_list) =>
      terms_list.fold(
        fn(acc : Range, item : Ranged[Term]) -> Range {
          Range::merge_range(acc, item.get_range())
        },
        init=predicate_range,
      )
  }
}

///|
pub impl HasRange for Clause with get_range(self : Clause) -> Range {
  let head_range = self.head.get_range()
  match self.body {
    None => head_range
    Some(body_list) =>
      body_list.fold(
        fn(acc : Range, item : Ranged[Literal]) -> Range {
          Range::merge_range(acc, item.get_range())
        },
        init=head_range,
      )
  }
}

///|
pub impl HasRange for Statement with get_range(self : Statement) -> Range {
  match self {
    AssertS(ranged_clause) => ranged_clause.get_range()
    QueryS(ranged_literal) => ranged_literal.get_range()
  }
}
