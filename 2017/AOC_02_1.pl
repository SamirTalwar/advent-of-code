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

largest(List, Value) :-
  select(Value, List, Rest),
  \+ (
    member(Another, Rest),
    Value < Another
  ).

smallest(List, Value) :-
  select(Value, List, Rest),
  \+ (
    member(Another, Rest),
    Value > Another
  ).
