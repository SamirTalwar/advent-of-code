% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/io').
:- use_module('helpers/numbers').

main :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse, Lines, UnsortedScanners),
  predsort(sort_scanners, UnsortedScanners, Scanners),
  !,
  natural(Delay),
  forall(member(Scanner, Scanners), \+ caught(Delay, Scanner)),
  format("~p\n", [Delay]).

caught(Delay, Depth - Range) :-
  Period is (Range - 1) * 2,
  0 is (Depth + Delay) mod Period.

parse(Line, Depth - Range) :-
  regex("^(\\d+): (\\d+)$", [], Line, Captures),
  [DepthString, RangeString] = Captures,
  number_codes(Depth, DepthString),
  number_codes(Range, RangeString).

sort_scanners(Delta, ADepth - ARange, BDepth - BRange) :-
  AOddness is (ADepth + ARange) mod 2,
  BOddness is (BDepth + BRange) mod 2,
  (
    (AOddness = 0, BOddness = 0; AOddness = 1, BOddness = 1)
    ->  compare(RangeDelta, ARange, BRange),
        ( RangeDelta = (=)
          ->  compare(Delta, ADepth, BDepth)
          ;   Delta = RangeDelta)
    ;   ( AOddness = 1
          ->  Delta = (<)
          ;   Delta = (>))
  ).
