% vim: set syntax=prolog

:- module(number_helpers, [
  natural/1,
  naturals/1
]).

naturals(Ns) :- naturals(0, Ns).
naturals(Count, Ns) :-
  freeze(Ns, (
    Ns=[Count];
    Ns=[Count | Tail],
    Incremented is Count + 1,
    naturals(Incremented, Tail))).

natural(N) :-
  naturals(Ns),
  member(N, Ns).
