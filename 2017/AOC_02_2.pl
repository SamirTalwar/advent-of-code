% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').

main :-
  current_input(S),
  read_number_table(S, Table),
  maplist(evenly_divisible, Table, Divided),
  sum_list(Divided, Sum),
  print(Sum).

evenly_divisible(List, Divided) :-
  select(A, List, Rest),
  member(B, Rest),
  0 is A mod B,
  Divided is A // B.
