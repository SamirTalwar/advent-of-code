% vim: set syntax=prolog

:- module(execution_helpers, [
  times/4,
  times_with_repetition/4
]).

times(0, _, Output, Output).
times(Times, Goal, Input, Output) :-
  call(Goal, Input, Accumulator),
  !,
  NewTimes is Times - 1,
  times(NewTimes, Goal, Accumulator, Output).

times_with_repetition(Times, Goal, Input, Output) :- times_with_repetition_(Times, Goal, [Input], Output).
times_with_repetition_(0, _, [Output | _], Output).
times_with_repetition_(Times, Goal, Past, Output) :-
  [Input | _] = Past,
  call(Goal, Input, Accumulator),
  !,
  NewTimes is Times - 1,
  (
    nth0(Index, Past, Accumulator)
    ->  Final is (Index - (NewTimes mod (Index + 1))),
        nth0(Final, Past, Output)
    ;   times_with_repetition_(NewTimes, Goal, [Accumulator | Past], Output)
  ).
