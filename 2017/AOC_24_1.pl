% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').

main :-
  Start = 0,
  read_input(Components),
  findall(Strength, (
    path(Components, Start, Path),
    strength(Path, Strength)
  ), Strengths),
  max_list(Strengths, Strength),
  format("~p\n", [Strength]).

path(Components, Current, Path) :-
  path(Components, Current, [], Reversed),
  reverse(Reversed, Path).
path(_, _, Result, Result).
path(Components, Current, Path, Result) :-
  select(Selected, Components, Rest),
  (
    Current - Next = Selected;
    Next - Current = Selected
  ),
  path(Rest, Next, [Selected | Path], Result).

strength(Path, Strength) :-
  maplist(component_strength, Path, Strengths),
  sumlist(Strengths, Strength).
component_strength(A - B, Strength) :-
  Strength is A + B.

read_input(Components) :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse_line, Lines, Components).

parse_line(Line, A - B) :-
  split_string(Line, "/", "", [AString, BString]),
  number_codes(A, AString),
  number_codes(B, BString).
