///|
pub(all) struct Loc {
  mut line : Int
  mut column : Int
  mut index : Int
} derive(Hash)

///|
pub fn Loc::empty() -> Loc {
  Loc::{ line: 0, column: 0, index: -1 }
}

///|
pub impl Eq for Loc with equal(self, other) {
  self.index == other.index
}

///|
pub impl Compare for Loc with compare(self, other) {
  if self.index < other.index {
    -1
  } else if self.index > other.index {
    1
  } else {
    0
  }
}

///|
pub(all) struct Interval {
  start : Loc
  end : Loc
} derive(Show, Hash, Eq)

///|
pub fn Interval::new(start : Loc, end : Loc) -> Interval {
  Interval::{ start, end }
}

///|
pub fn Interval::merge(self : Interval, other : Interval) -> Interval {
  let start = if self.start.index < other.start.index {
    self.start
  } else {
    other.start
  }
  let end = if self.end.index > other.end.index { self.end } else { other.end }
  Interval::{ start, end }
}

///|
pub(all) enum Range {
  NoRange
  Range(Array[Interval]) // a sequence of consecutive intervals
} derive(Show, Hash, Eq)

///|
pub(open) trait HasRange {
  get_range(self : Self) -> Range
}

///|
pub(all) struct Ranged[T] {
  range : Range
  inner : T
}

///|
pub impl Show for Loc with output(self, logger) {
  logger.write_string(
    "line \{self.line}, column \{self.column}, index \{self.index}",
  )
}

///|
pub impl[T : Show] Show for Ranged[T] with output(self, logger) {
  Show::output(self.inner, logger)
}

///|
pub fn Loc::to_string(self : Loc) -> String {
  Show::to_string(self)
}

///|
pub fn Range::empty() -> Range {
  Range::NoRange
}

///|
pub fn Range::new(intervals : Array[Interval]) -> Range {
  if intervals.length() == 0 {
    Range::NoRange
  } else {
    Range::Range(intervals)
  }
}

///|
pub fn[T] Ranged::new(inner : T, range : Range) -> Ranged[T] {
  Ranged::{ inner, range }
}

///|
pub fn[T] Ranged::new_with_interval(
  inner : T,
  interval : Interval,
) -> Ranged[T] {
  Ranged::{ inner, range: Range::new([interval]) }
}

///|
pub impl HasRange for Interval with get_range(self) {
  Range::Range([self])
}

///|
pub impl HasRange for Range with get_range(self) {
  self
}

///|
pub impl[T] HasRange for Ranged[T] with get_range(self) {
  self.range
}

///|
pub impl[A : HasRange] HasRange for Array[A] with get_range(self) {
  self.fold(
    fn(acc : Range, item : A) -> Range { Range::merge_range(acc, item) },
    init=NoRange,
  )
}

///|
pub impl[A : HasRange] HasRange for List[A] with get_range(self) {
  self.fold(
    fn(acc : Range, item : A) -> Range { Range::merge_range(acc, item) },
    init=NoRange,
  )
}

///|
pub impl[A : HasRange] HasRange for List1[A] with get_range(self) {
  self.fold(
    fn(acc : Range, item : A) -> Range { Range::merge_range(acc, item) },
    init=NoRange,
  )
}

///|
pub fn[A : HasRange, B : HasRange] Range::merge_range(u : A, v : B) -> Range {
  letrec concate = fn(
    xs : ArrayView[Interval],
    ys : ArrayView[Interval],
  ) -> Array[Interval] {
    let z = xs.to_array()
    z.append(ys.to_array())
    z
  }
  and merge_left = fn(
    s1 : Interval,
    r1 : ArrayView[Interval],
    s2 : Interval,
    r2 : ArrayView[Interval],
  ) -> Array[Interval] {
    let r1 = r1.to_array()
    let i = r1.search_by(fn(s) { not(s.end < s2.start) }).unwrap_or(r1.length())
    let (r1_left, r1_right) = r1.split_at(i)
    let z = [s1]
    z.append(r1_left)
    z.append(merge(r1_right, r2))
    z
  }
  and merge_some = fn(
    s1 : Interval,
    r1 : ArrayView[Interval],
    s2 : Interval,
    r2 : ArrayView[Interval],
  ) -> Array[Interval] {
    let r1 = r1.to_array()
    let i = r1.search_by(fn(s) { not(s.end <= s2.end) }).unwrap_or(r1.length())
    merge(r1[:i], [s1.merge(s2), ..r2])
  }
  and merge = fn(
    is1 : ArrayView[Interval],
    is2 : ArrayView[Interval],
  ) -> Array[Interval] {
    match (is1, is1, is2, is2) {
      ([], _, _, _) => is2.to_array()
      (_, _, [], _) => is1.to_array()
      ([s1, .. r1], [.. l1, e1], [s2, .. r2], [.. l2, e2]) =>
        if e1.end < s2.start {
          // no overlap, just concatenate
          concate(is1, is2)
        } else if e2.end < s1.start {
          // no overlap, just concatenate
          concate(is2, is1)
        } else if e1.end == s2.start {
          // merge touching
          concate(l1, concate([Interval::new(s1.start, e2.end)], r2))
        } else if e2.end == s1.start {
          // merge touching
          concate(l2, concate([Interval::new(s2.start, e1.end)], r1))
        } else if s1.end < s2.start {
          // general cases
          merge_left(s1, r1, s2, r2)
        } else if s2.end < s1.start {
          // general cases
          merge_left(s2, r2, s1, r1)
        } else if s1.end < s2.end {
          merge_some(s1, r1, s2, r2)
        } else {
          merge_some(s2, r2, s1, r1)
        }
      _ => abort("should not happen: merge_range uncovered cases")
    }
  }

  match (u.get_range(), v.get_range()) {
    (NoRange, is2) => is2
    (is1, NoRange) => is1
    (Range(is1), Range(is2)) => Range::new(merge(is1, is2))
  }
}
