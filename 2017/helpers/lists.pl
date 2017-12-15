% vim: set syntax=prolog

:- module(list_helpers, [
  enumerate/2,
  member_of/2,
  pair/3,
  repeated/2,
  zip/3
]).

:- use_module(library(pairs)).
:- use_module('numbers').

enumerate(List, Enumerated) :-
  naturals(Indices),
  pairs_keys_values(Enumerated, Indices, List).

member_of(List, Element) :-
  member(Element, List).

pair(A, B, A - B).

repeated(Value, List) :-
  freeze(List, (
    List=[];
    List=[Value | Tail],
    repeated(Value, Tail))).

zip(As, Bs, Pairs) :-
  maplist(pair, As, Bs, Pairs).
