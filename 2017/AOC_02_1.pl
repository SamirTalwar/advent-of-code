% vim: set syntax=prolog

main :-
  current_input(S),
  read_table(S, Table),
  maplist(difference, Table, Differences),
  sum(Differences, Sum),
  print(Sum).

difference(List, Difference) :-
  largest(List, Largest),
  smallest(List, Smallest),
  Difference is Largest - Smallest.

largest([H | T], Result) :- largest(T, H, Result).
largest([], Result, Result).
largest([H | T], Current, Result) :-
  H > Current
  ->  largest(T, H, Result)
  ;   largest(T, Current, Result).

smallest([H | T], Result) :- smallest(T, H, Result).
smallest([], Result, Result).
smallest([H | T], Current, Result) :-
  H < Current
  ->  smallest(T, H, Result)
  ;   smallest(T, Current, Result).
