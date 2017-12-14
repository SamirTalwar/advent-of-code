% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse, Lines, Programs, NestedFlows),
  flatten(NestedFlows, UnsortedFlows),
  sort(UnsortedFlows, Flows),
  forall(member(A - B, Flows), asserta( (known_flow(A, B)) )),
  !,
  groups(Programs, Groups),
  length(Groups, Count),
  format("~p\n", [Count]).

groups(Programs, Groups) :-
  groups(Programs, [], Groups).
groups([], Output, Output).
groups([Program | Programs], Groups, Output) :-
  (member(ExistingGroup, Groups), member(Program, ExistingGroup))
  ->  groups(Programs, Groups, Output)
  ;   group(Program, Connections),
      groups(Programs, [Connections | Groups], Output).

group(From, Connections) :-
  findall(To, flow(From, To), Connections).

flow(From, To) :-
  flow(From, To, [From], [From]).
flow(_, Program, [Program | _], _).
flow(Start, To, [From | Queue], Seen) :-
  findall(X, (known_flow(From, X), \+ member(X, Seen)), Next),
  repeated(Start, Starts),
  zip(Starts, Next, NewFlows),
  forall((member(A - B, NewFlows), \+ known_flow(A, B)), asserta( (known_flow(A, B)) )),
  append(Queue, Next, NewQueue),
  append(Seen, Next, NewSeen),
  flow(Start, To, NewQueue, NewSeen).

parse(Line, From, Flows) :-
  regex("(\\d+) <-> (\\d+)(, (\\d+))*", [], Line, Captures),
  [FromString | ToStrings] = Captures,
  number_codes(From, FromString),
  drop_odd(ToStrings, ValidToStrings),
  maplist(number_codes, Tos, ValidToStrings),
  repeated(From, Froms),
  zip(Froms, Tos, Flows).

drop_odd([], []).
drop_odd([A], [A]).
drop_odd([A, _ | Rest], [A | Output]) :- drop_odd(Rest, Output).
