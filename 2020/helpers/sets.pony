use mut = "collections"
use "collections/persistent"
use "itertools"

class ToSet[A: (mut.Hashable val & Equatable[A])]
  fun val from_iterator(iterator: Iterator[A]): Set[A] =>
    from_iter(Iter[A](iterator))

  fun val from_iter(iterator: Iter[A]): Set[A] =>
    iterator.fold[Set[A]](Set[A], { (set, value) => set.add(value) })
