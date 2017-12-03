% vim: set syntax=prolog

main :-
  current_input(S),
  read_table(S, Table),
  maplist(evenly_divisible, Table, Divided),
  sum(Divided, Sum),
  print(Sum).

evenly_divisible(List, Divided) :-
  select(A, List, Rest),
  member(B, Rest),
  0 is A mod B,
  Divided is A // B.
