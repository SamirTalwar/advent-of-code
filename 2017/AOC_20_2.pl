% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/execution').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_lines(S, Lines),
  enumerate(Lines, EnumeratedLines),
  maplist(parse, EnumeratedLines, Particles),
  times(100, iterate, Particles, Result),
  length(Result, Count),
  print(Count).

iterate(Input, Output) :-
  maplist(step, Input, Next),
  remove_collisions(Next, Output).

step(particle(I, position( PX,  PY,  PZ), velocity( VX,  VY,  VZ), acceleration(AX, AY, AZ)),
     particle(I, position(NPX, NPY, NPZ), velocity(NVX, NVY, NVZ), acceleration(AX, AY, AZ))) :-
  NVX is VX + AX,
  NVY is VY + AY,
  NVZ is VZ + AZ,
  NPX is PX + NVX,
  NPY is PY + NVY,
  NPZ is PZ + NVZ.

remove_collisions(Input, Output) :- remove_collisions(Input, [], Output).
remove_collisions([], Output, Output).
remove_collisions([Current | Next], Previous, Output) :-
  particle(_, Position, _, _) = Current,
  exclude(has_position(Position), Next, FilteredNext),
  (
    (length(Next, Length), length(FilteredNext, Length))
    ->  remove_collisions(Next, [Current | Previous], Output)
    ;   remove_collisions(FilteredNext, Previous, Output)
  ).

has_position(Position, particle(_, Position, _, _)).

parse(I - Line, particle(I, position(PX, PY, PZ), velocity(VX, VY, VZ), acceleration(AX, AY, AZ))) :-
  regex("^p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>$", [], Line, CaptureStrings),
  maplist(number_codes, [PX, PY, PZ, VX, VY, VZ, AX, AY, AZ], CaptureStrings).
