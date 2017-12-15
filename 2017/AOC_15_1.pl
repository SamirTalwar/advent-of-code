% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_generator(S, A),
  read_generator(S, B),
  count(40000000, A, B, Count),
  format("~p\n", [Count]).

read_generator(S, Name - StartingValue) :-
  read_line_to_codes(S, Line),
  regex("^Generator (\\w+) starts with (\\d+)$", [], Line, Captures),
  [NameCodes, StartingValueCodes] = Captures,
  string_codes(Name, NameCodes),
  number_codes(StartingValue, StartingValueCodes).

count(Times, A, B, Count) :-
  count(Times, A, B, 0, Count).
count(0, _, _, Count, Count).
count(Times, A, B, Count, Result) :-
  NewTimes is Times - 1,
  next(A, NextA),
  next(B, NextB),
  !,
  (
    judge(NextA, NextB)
    ->  NewCount is Count + 1,
        count(NewTimes, NextA, NextB, NewCount, Result)
    ;   count(NewTimes, NextA, NextB, Count, Result)
  ).

next("A" - PreviousValue, "A" - NextValue) :-
  NextValue is (PreviousValue * 16807) mod 2147483647.
next("B" - PreviousValue, "B" - NextValue) :-
  NextValue is (PreviousValue * 48271) mod 2147483647.

judge("A" - AValue, "B" - BValue) :-
  AComparison is AValue /\ 0xFFFF,
  BComparison is BValue /\ 0xFFFF,
  AComparison = BComparison.
