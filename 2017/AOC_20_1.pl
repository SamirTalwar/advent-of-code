% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_lines(S, Lines),
  enumerate(Lines, EnumeratedLines),
  maplist(parse, EnumeratedLines, Particles),
  min(acceleration_value, Particle, Particles),
  print(Particle).

acceleration_value(particle(_, _, _, acceleration(X, Y, Z)), Value) :-
  Value is sqrt(X ^ 2 + Y ^ 2 + Z ^ 2).

parse(I - Line, particle(I, position(PX, PY, PZ), velocity(VX, VY, VZ), acceleration(AX, AY, AZ))) :-
  regex("^p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>$", [], Line, CaptureStrings),
  maplist(number_codes, [PX, PY, PZ, VX, VY, VZ, AX, AY, AZ], CaptureStrings).
