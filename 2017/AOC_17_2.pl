% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/execution').
:- use_module('helpers/io').

main :-
  ValueAfterZero = 0,
  ZeroPosition = 0,
  Position = 0,
  FirstValue = 1,
  Iterations = 50000000,
  current_input(S),
  read_number(S, Steps),
  times(Iterations,
        iterate(Steps),
        ValueAfterZero - ZeroPosition - Position - FirstValue,
        Result - _ - _ - _),
  format("~p\n", [Result]).

iterate(Steps,
        InputValueAfterZero - InputZeroPosition - InputPosition - Value,
        OutputValueAfterZero - OutputZeroPosition - OutputPosition - NextValue) :-
  NextValue is Value + 1,
  Length = Value,
  OutputPosition is (InputPosition + Steps) mod Length + 1,
  (
    OutputPosition = InputZeroPosition
    ->  OutputValueAfterZero = InputValueAfterZero,
        OutputZeroPosition is InputZeroPosition + 1
    ; OutputPosition is InputZeroPosition + 1
    ->  OutputValueAfterZero = Value,
        OutputZeroPosition = InputZeroPosition
    ;   OutputValueAfterZero = InputValueAfterZero,
        OutputZeroPosition = InputZeroPosition
  ).
