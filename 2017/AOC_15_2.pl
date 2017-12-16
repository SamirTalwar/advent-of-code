% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/execution').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_generator(S, A),
  read_generator(S, B),
  count(5000000, A, B, Count),
  format("~p\n", [Count]).

read_generator(S, Name - StartingValue) :-
  read_line_to_codes(S, Line),
  regex("^Generator (\\w+) starts with (\\d+)$", [], Line, Captures),
  [NameCodes, StartingValueCodes] = Captures,
  string_codes(Name, NameCodes),
  number_codes(StartingValue, StartingValueCodes).

count(Times, A, B, Count) :-
  times(Times, count_passes, A - B - 0, _ - _ - Count).

count_passes(A - B - Count, NextA - NextB - NewCount) :-
  next(A, NextA),
  next(B, NextB),
  (
    judge(NextA, NextB)
    ->  NewCount is Count + 1
    ;   NewCount = Count
  ).

next("A" - PreviousValue, "A" - NextValue) :-
  Value is (PreviousValue * 16807) mod 2147483647,
  Suitable is Value mod 4,
  (
    Suitable = 0
    ->  NextValue = Value
    ;   next("A" - Value, "A" - NextValue)
  ).
next("B" - PreviousValue, "B" - NextValue) :-
  Value is (PreviousValue * 48271) mod 2147483647,
  Suitable is Value mod 8,
  (
    Suitable = 0
    ->  NextValue = Value
    ;   next("B" - Value, "B" - NextValue)
  ).

judge("A" - AValue, "B" - BValue) :-
  AComparison is AValue /\ 0xFFFF,
  BComparison is BValue /\ 0xFFFF,
  AComparison = BComparison.
