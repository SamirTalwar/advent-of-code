class Fold[A :A]
  let _iterator: Iterator[A]

  new create(iterator: Iterator[A]) =>
    _iterator = iterator

  fun ref through1(default: A, accumulate: { (A!, A!): A^ }): A =>
    try
      var accumulator = _iterator.next()?
      for value in _iterator do
        accumulator = accumulate(accumulator, value)
      end
      consume accumulator
    else
      consume default
    end
