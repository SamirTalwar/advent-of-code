% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(pairs)).
:- use_module(library(regex)).
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse, Lines, Nodes),
  flatten(Nodes, Edges),
  root(Edges, Root),
  format("~s\n", Root).

parse(Line, Edges) :-
  regex("(\\w+) \\((\\d+)\\)( -> ([a-z, ]+))?", [], Line, Captures),
  (
    [ACodes, _, _, StringOfBs] = Captures,
    string_codes(A, ACodes)
    ->  split_string(StringOfBs, ", ", "", SplitBs),
        exclude(=(""), SplitBs, Bs),
        repeated(A, As),
        pairs_keys_values(Edges, As, Bs)
    ;   Edges = []
  ).

root(Edges, Root) :-
  pairs_keys_values(Edges, As, Bs),
  member(Root, As),
  \+ member(Root, Bs).
