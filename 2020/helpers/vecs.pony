use collections = "collections/persistent"
use "itertools"

class ToVec[A: Any #share]
  fun val from_iterator(iterator: Iterator[A]): collections.Vec[A] =>
    from_iter(Iter[A](iterator))

  fun val from_iter(iterator: Iter[A]): collections.Vec[A] =>
    iterator.fold[collections.Vec[A]](collections.Vec[A], { (vec, value) => vec.push(value) })
