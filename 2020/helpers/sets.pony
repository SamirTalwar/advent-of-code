use collections_mut = "collections"
use collections = "collections/persistent"
use "itertools"

class ToSet[A: (collections_mut.Hashable val & Equatable[A])]
  fun val from_iterator(iterator: Iterator[A]): collections.Set[A] =>
    from_iter(Iter[A](iterator))

  fun val from_iter(iterator: Iter[A]): collections.Set[A] =>
    iterator.fold[collections.Set[A]](collections.Set[A], { (set, value) => set.add(value) })
