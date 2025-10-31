///|
priv struct ArrayIterator[T] {
  arr : Array[T]
  mut cur : T?
  mut index : Int
}

///|
fn[T] ArrayIterator::from(arr : Array[T]) -> ArrayIterator[T] {
  ArrayIterator::{ arr, cur: None, index: 0 }
}

///|
fn[T] ArrayIterator::next(self : ArrayIterator[T]) -> T? {
  if self.index < self.arr.length() {
    let result = self.arr[self.index]
    self.index += 1
    Some(result)
  } else {
    None
  }
}

///|
fn[T] ArrayIterator::iterate(self : ArrayIterator[T]) -> Unit {
  self.cur = self.next()
}

///|
fn[T] ArrayIterator::get(self : ArrayIterator[T]) -> T? {
  self.cur
}

///|
fn[T] ArrayIterator::reset(self : ArrayIterator[T]) -> Unit {
  self.index = 0
}

///|
fn[T] ArrayIterator::in_progress(self : ArrayIterator[T]) -> Bool {
  self.cur is Some(_)
}

///|
priv enum MultiProductIterState {
  StartOfIter
  MidIter(Bool)
}

///|
priv struct MultiProductIterator {
  multi_iter : Array[ArrayIterator[Literal]]
}

///|
fn MultiProductIterator::iterate_last(
  multi_iter : Array[ArrayIterator[Literal]],
  state : MultiProductIterState,
  last_index : Int,
) -> Bool {
  if last_index < 0 {
    match state {
      StartOfIter => return false
      MidIter(on_first_iter) => return on_first_iter
    }
  }
  let last_iter = multi_iter[last_index]
  let mut state = state
  let on_first_iter = match state {
    StartOfIter => {
      let on_first_iter = not(last_iter.in_progress())
      state = MidIter(on_first_iter)
      on_first_iter
    }
    MidIter(on_first_iter) => on_first_iter
  }
  // try iterate last iterator
  if not(on_first_iter) {
    last_iter.iterate()
  }
  if last_iter.in_progress() {
    return true
  } else if MultiProductIterator::iterate_last(
      multi_iter,
      state,
      last_index - 1,
    ) {
    // reset last iterator
    last_iter.reset()
    last_iter.iterate()
    return last_iter.in_progress()
  } else {
    return false
  }
}

///|
fn MultiProductIterator::curr_iterator(
  self : MultiProductIterator,
) -> Array[Literal] {
  let result = []
  for i = 0; i < self.multi_iter.length(); {
    let cur = self.multi_iter[i].get().unwrap()
    result.push(cur)
    continue i + 1
  }
  result
}

///|
fn MultiProductIterator::next(self : MultiProductIterator) -> Array[Literal]? {
  if MultiProductIterator::iterate_last(
      self.multi_iter,
      StartOfIter,
      self.multi_iter.length() - 1,
    ) {
    Some(self.curr_iterator())
  } else {
    None
  }
}
