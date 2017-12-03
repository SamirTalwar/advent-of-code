% vim: set syntax=prolog

integers(0).
integers(X) :-
  integers(Y),
  X is Y + 1.

sum(Values, Result) :- sum(Values, 0, Result).
sum([], Result, Result).
sum([V | Vs], Accumulator, Result) :-
  Next is Accumulator + V,
  sum(Vs, Next, Result).
