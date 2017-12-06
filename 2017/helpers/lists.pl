% vim: set syntax=prolog

:- module(list_helpers, [
  enumerate/2,
  pair/3,
  zip/3
]).

:- use_module(library(pairs)).
:- use_module('numbers').

enumerate(List, Enumerated) :-
  naturals(Indices),
  pairs_keys_values(Enumerated, Indices, List).

pair(A, B, [A, B]).

zip(As, Bs, Pairs) :-
  maplist(pair, As, Bs, Pairs).
