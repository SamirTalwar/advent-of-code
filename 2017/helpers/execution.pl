% vim: set syntax=prolog

:- module(execution_helpers, [
  times/4
]).

times(0, _, Output, Output).
times(Times, Goal, Input, Output) :-
  call(Goal, Input, Accumulator),
  !,
  NewTimes is Times - 1,
  times(NewTimes, Goal, Accumulator, Output).
