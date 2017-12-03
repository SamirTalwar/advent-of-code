% vim: set syntax=prolog

sum(Values, Result) :- sum(Values, 0, Result).
sum([], Result, Result).
sum([V | Vs], Accumulator, Result) :-
  Next is Accumulator + V,
  sum(Vs, Next, Result).
