% vim: set syntax=prolog

:- module(number_helpers, [
  natural/1,
  naturals/1
]).

naturals(List) :- naturals(0, List).
naturals(Count, List) :-
  freeze(List, (
    List=[Count];
    List=[Count | Tail],
    Incremented is Count + 1,
    naturals(Incremented, Tail))).

natural(0).
natural(X) :-
  natural(Y),
  X is Y + 1.
