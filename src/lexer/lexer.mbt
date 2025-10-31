///|
typealias @hashmap.HashMap as MutMap

///|
typealias @util.Loc

///|
typealias @util.Interval

///|
priv suberror InvalidCharcode (Int, Loc)

///|
pub struct Lexer {
  buffer : String
  mut index : Int
  mut colstart : Int
  mut line : Int
} derive(Show)

///|
pub enum Keyword {
  True
  False
} derive(Hash, Eq, Show)

///|
typealias @parser.Token

/// See https://github.com/agda/agda/blob/master/src/full/Agda/Syntax/Parser/Layout.hs
/// to allow stacking of layout keywords on the same line.

///|
let keywords : MutMap[String, Keyword] = @hashmap.new()

///|
fn init {
  keywords["True"] = Keyword::True
  keywords["False"] = Keyword::False
}

///|
pub fn Lexer::new(s : String) -> Lexer {
  let buffer = StringBuilder::new(size_hint=s.length() + 1)
  buffer.write_string(s)
  buffer.write_char(Int::unsafe_to_char(0))
  Lexer::{ buffer: buffer.to_string(), index: 0, colstart: 0, line: 1 }
}

///|
pub fn Lexer::reset(self : Lexer) -> Unit {
  self.index = 0
  self.colstart = 0
  self.line = 1
}

///|
fn Lexer::move_to_newline(self : Lexer) -> Unit {
  self.colstart = self.index
  self.line += 1
}

///|
fn Lexer::get_loc(self : Lexer) -> Loc {
  { line: self.line, column: self.index - self.colstart + 1, index: self.index }
}

///|
fn Lexer::make_error(loc : Loc, msg : String) -> (Token, Interval) {
  (Token::ErrorMsg((loc, msg)), Interval::new(loc, loc))
}

///|
fn Lexer::make_span(self : Lexer, start : Loc) -> String {
  self.buffer.unsafe_substring(start=start.index, end=self.index)
}

///|
fn Lexer::make_interval(self : Lexer, start : Loc) -> Interval {
  Interval::new(start, self.get_loc())
}

///|
fn Lexer::eof() -> Token {
  Token::Eof
}

///|
fn Lexer::is_keyword(s : String) -> Keyword? {
  keywords.get(s)
}

///|
fn Lexer::ch(self : Lexer) -> Char raise InvalidCharcode {
  let charcode = self.buffer.at(self.index)
  match Int::to_char(charcode) {
    Some(c) => c
    None => raise InvalidCharcode((charcode, self.get_loc()))
  }
}

///|
fn Lexer::string_literal(
  self : Lexer,
  loc : Loc,
) -> (Token, Interval) raise InvalidCharcode {
  self.index += 1
  let buffer = @buffer.new()
  while true {
    match self.ch() {
      '\u{00}' => return Lexer::make_error(loc, "early EOF in string literal")
      '\n' => {
        self.index += 1
        self.move_to_newline()
        return Lexer::make_error(loc, "newline in string literal")
      }
      '"' => break
      '\\' => {
        self.index += 1
        match self.ch() {
          'n' => {
            buffer.write_char('\n')
            self.index += 1
          }
          'r' => {
            buffer.write_char('\r')
            self.index += 1
          }
          't' => {
            buffer.write_char('\t')
            self.index += 1
          }
          '\\' => {
            buffer.write_char('\\')
            self.index += 1
          }
          '"' => {
            buffer.write_char('"')
            self.index += 1
          }
          'x' => {
            // Hexadecimal escape sequence
            self.index += 1
            let start = self.index
            self.index += 2
            let value = @strconv.parse_int(
              self.buffer.unsafe_substring(start~, end=self.index),
              base=16,
            ) catch {
              StrConvError(msg) => return Lexer::make_error(loc, msg)
            }
            let char = match Int::to_char(value) {
              Some(c) => c
              None => raise InvalidCharcode((value, self.get_loc()))
            }
            buffer.write_char(char)
          }
          'o' => {
            // Octal escape sequence
            self.index += 1
            let start = self.index
            self.index += 3
            let value = @strconv.parse_int(
              self.buffer.unsafe_substring(start~, end=self.index),
              base=8,
            ) catch {
              StrConvError(msg) => return Lexer::make_error(loc, msg)
            }
            let char = match Int::to_char(value) {
              Some(c) => c
              None => raise InvalidCharcode((value, self.get_loc()))
            }
            buffer.write_char(char)
          }
          'u' => {
            // Unicode escape sequence
            self.index += 1
            let value = match self.ch() {
              '{' => {
                self.index += 1
                let start = self.index
                while self.ch() != '}' {
                  self.index += 1
                  if self.index > start + 4 {
                    return Lexer::make_error(
                      loc, "invalid unicode escape sequence: too long",
                    )
                  }
                }
                let value = @strconv.parse_int(
                  self.buffer.unsafe_substring(start~, end=self.index),
                  base=16,
                ) catch {
                  StrConvError(msg) => return Lexer::make_error(loc, msg)
                }
                match self.ch() {
                  '}' => self.index += 1
                  _ =>
                    return Lexer::make_error(
                      loc, "invalid unicode escape sequence: unmatched '{'",
                    )
                }
                value
              }
              '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => {
                let start = self.index
                self.index += 4
                let value = @strconv.parse_int(
                  self.buffer.unsafe_substring(start~, end=self.index),
                  base=16,
                ) catch {
                  StrConvError(msg) => return Lexer::make_error(loc, msg)
                }
                value
              }
              _ =>
                return Lexer::make_error(loc, "invalid unicode escape sequence")
            }
            let char = match Int::to_char(value) {
              Some(c) => c
              None => raise InvalidCharcode((value, self.get_loc()))
            }
            buffer.write_char(char)
          }
          _ => {
            self.index += 1
            return Lexer::make_error(loc, "invalid escape sequence")
          }
        }
      }
      _ => {
        buffer.write_char(self.ch())
        self.index += 1
      }
    }
  }
  self.index += 1
  let interval = self.make_interval(loc)
  (Token::StringLiteral((interval, buffer.to_string())), interval)
}

///|
fn Lexer::numeric_literal(
  self : Lexer,
  begin : Loc,
) -> (Token, Interval) raise InvalidCharcode {
  let start = self.index
  let negativ_flag = if self.ch() == '-' {
    self.index += 1
    true
  } else {
    false
  }
  self.index += 1
  while true {
    let n = self.ch()
    if n < '0' || n > '9' {
      break
    }
    self.index += 1
  }
  try {
    let value = @strconv.parse_int(
      self.buffer.unsafe_substring(start~, end=self.index),
    )
    let interval = self.make_interval(begin)
    let value = if negativ_flag { -value } else { value }
    (Token::IntLiteral((interval, value)), interval)
  } catch {
    StrConvError(msg) => Lexer::make_error(begin, msg)
  }
}

///|
fn Lexer::ident(
  self : Lexer,
  loc : Loc,
) -> (Token, Interval) raise InvalidCharcode {
  while true {
    let cond = (self.ch() >= 'a' && self.ch() <= 'z') ||
      (self.ch() >= 'A' && self.ch() <= 'Z') ||
      self.ch() == '_' ||
      (self.ch() >= '0' && self.ch() <= '9')
    if not(cond) {
      break
    }
    self.index += 1
  }
  let interval = self.make_interval(loc)
  let span = self.make_span(loc)
  let tok = Token::Id((interval, span))
  let idkw = Lexer::is_keyword(span).map_or(tok, fn(kw) {
    match kw {
      Keyword::True => Token::KwTrue(interval)
      Keyword::False => Token::KwFalse(interval)
    }
  })
  (idkw, interval)
}

///|
fn Lexer::variable(
  self : Lexer,
  loc : Loc,
) -> (Token, Interval) raise InvalidCharcode {
  self.index += 1
  while true {
    let cond = (self.ch() >= 'a' && self.ch() <= 'z') ||
      (self.ch() >= 'A' && self.ch() <= 'Z') ||
      self.ch() == '_' ||
      (self.ch() >= '0' && self.ch() <= '9')
    if not(cond) {
      break
    }
    self.index += 1
  }
  let interval = self.make_interval(loc)
  let span = self.make_span(loc)
  (Token::Variable((interval, span)), interval)
}

///|
fn Lexer::skip_inline_whitespace(self : Lexer) -> Unit raise InvalidCharcode {
  while true {
    let c = self.ch()
    if c == '\u{00}' || c == '\n' {
      break
    } else if c == ' ' || c == '\t' || c == '\r' {
      self.index += 1
      continue
    } else {
      break
    }
  }
}

///|
fn Lexer::skip_comment(self : Lexer) -> Unit raise InvalidCharcode {
  self.index += 1
  while true {
    if self.ch() == '\u{00}' {
      break
    } else if self.ch() == '\n' {
      break
    }
    self.index += 1
  }
}

///|
fn Lexer::operator(
  self : Lexer,
  loc : Loc,
  first_c : Char,
) -> (Token, Interval) raise InvalidCharcode {
  self.index += 1
  match first_c {
    // reserve single-character operator
    '.' => (Token::SymDot(Interval::new(loc, loc)), Interval::new(loc, loc))
    '?' =>
      (Token::SymQuestion(Interval::new(loc, loc)), Interval::new(loc, loc))
    ',' => (Token::SymComma(Interval::new(loc, loc)), Interval::new(loc, loc))
    '=' => (Token::SymEqual(Interval::new(loc, loc)), Interval::new(loc, loc))
    '(' =>
      (Token::SymOpenParen(Interval::new(loc, loc)), Interval::new(loc, loc))
    ')' =>
      (Token::SymCloseParen(Interval::new(loc, loc)), Interval::new(loc, loc))
    ':' if self.ch() == '-' => {
      self.index += 1
      (Token::SymColonDash(Interval::new(loc, loc)), Interval::new(loc, loc))
    }
    _ => Lexer::make_error(loc, "invalid operator")
  }
}

///|
pub fn Lexer::next_err(self : Lexer) -> (Token, Interval) raise Failure {
  match self.next() {
    (ErrorMsg((_, msg)), _) => fail(msg)
    _ as tok => tok
  }
}

///|
fn Lexer::next(self : Lexer) -> (Token, Interval) {
  try {
    while true {
      let c = self.ch()
      let loc = self.get_loc()
      match c {
        '\u{00}' => return (Lexer::eof(), Interval::new(loc, loc))
        ' ' | '\t' | '\r' => {
          self.skip_inline_whitespace()
          continue
        }
        '\n' => {
          self.index += 1
          self.move_to_newline()
          continue
        }
        '%' => {
          self.skip_comment()
          continue
        }
        '"' => return self.string_literal(loc)
        '-' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          return self.numeric_literal(loc)
        ':' | '=' | '.' | '(' | ')' | '!' | '?' | ',' =>
          return self.operator(loc, c)
        _ =>
          if c >= 'A' && c <= 'Z' {
            return self.variable(loc)
          } else if c >= 'a' && c <= 'z' {
            return self.ident(loc)
          } else {
            return Lexer::make_error(loc, "invalid character: \{c}")
          }
      }
    } else {
      return Lexer::make_error(self.get_loc(), "unreachable")
    }
  } catch {
    InvalidCharcode((charcode, loc)) =>
      return Lexer::make_error(loc, "invalid character code: \{charcode}")
  }
}
