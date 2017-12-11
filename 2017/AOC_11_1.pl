% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').

main :-
  current_input(S),
  read_words(S, ",", MovementStrings),
  maplist(atom_string, Movements, MovementStrings),
  foldl(move, Movements, 0 - 0 - 0, X - Y - Z),
  format("Location: ~d, ~d, ~d\n", [X, Y, Z]),
  Distance is (abs(X) + abs(Y) + abs(Z)) / 2,
  format("~d\n", [Distance]).

move(ne, X - Y - Z, X - NewY - NewZ) :-
  NewY is Y - 1,
  NewZ is Z + 1.
move(sw, X - Y - Z, X - NewY - NewZ) :-
  NewY is Y + 1,
  NewZ is Z - 1.
move(nw, X - Y - Z, NewX - Y - NewZ) :-
  NewX is X + 1,
  NewZ is Z - 1.
move(se, X - Y - Z, NewX - Y - NewZ) :-
  NewX is X - 1,
  NewZ is Z + 1.
move(n, X - Y - Z, NewX - NewY - Z) :-
  NewX is X + 1,
  NewY is Y - 1.
move(s, X - Y - Z, NewX - NewY - Z) :-
  NewX is X - 1,
  NewY is Y + 1.
