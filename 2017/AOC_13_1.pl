% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/io').

main :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse, Lines, Scanners),
  include(caught, Scanners, Caught),
  maplist(severity, Caught, Severities),
  sumlist(Severities, Severity),
  format("~p\n", [Severity]).

caught(Depth - Range) :-
  Period is (Range - 1) * 2,
  0 is Depth mod Period.

severity(Depth - Range, Severity) :-
  Severity is Depth * Range.

parse(Line, Depth - Range) :-
  regex("^(\\d+): (\\d+)$", [], Line, Captures),
  [DepthString, RangeString] = Captures,
  number_codes(Depth, DepthString),
  number_codes(Range, RangeString).
