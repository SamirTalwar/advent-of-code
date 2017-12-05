% vim: set syntax=prolog

:- module(numbers, [
  integers/1
]).

integers(0).
integers(X) :-
  integers(Y),
  X is Y + 1.
