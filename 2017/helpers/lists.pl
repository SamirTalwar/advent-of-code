% vim: set syntax=prolog

filter(Predicate, Input, Output) :-
  filter(Predicate, Input, [], Filtered),
  reverse(Filtered, Output).
filter(_, [], Filtered, Filtered).
filter(Predicate, [H | T], Filtered, Output) :-
  call(Predicate, H)
  ->  filter(Predicate, T, [H | Filtered], Output)
  ;   filter(Predicate, T, Filtered, Output).
