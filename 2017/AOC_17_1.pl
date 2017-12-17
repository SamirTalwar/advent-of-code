% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').

main :-
  InitialState = [0],
  Position = 0,
  findall(Value, between(1, 2017, Value), Values),
  current_input(S),
  read_number(S, Steps),
  foldl(iterate(Steps), Values, InitialState - Position, State - _),
  append([_, [2017, Result], _], State),
  format("~p\n", [Result]).

iterate(Steps, Value, InputState - InputPosition, OutputState - OutputPosition) :-
  length(InputState, Length),
  OutputPosition is (InputPosition + Steps) mod Length + 1,
  length(Before, OutputPosition),
  append(Before, After, InputState),
  append([Before, [Value], After], OutputState).
