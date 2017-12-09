% vim: set syntax=prolog

:- consult('helpers/run').

main :-
  current_input(S),
  read_line_to_codes(S, Input),
  remove_garbage(Input, Clean),
  score(Clean, Score),
  format("~d\n", [Score]).

remove_garbage(Input, Output) :-
  remove_garbage(Input, [], Accumulator),
  reverse(Accumulator, Output).
remove_garbage([], Accumulator, Accumulator).
remove_garbage([0'< | T], Accumulator, Output) :-
  removing_garbage(T, Removed),
  remove_garbage(Removed, Accumulator, Output).

remove_garbage([H | T], Accumulator, Output) :-
  remove_garbage(T, [H | Accumulator], Output).
removing_garbage([0'!, _ | T], Removed) :-
  removing_garbage(T, Removed).
removing_garbage([0'> | T], T).
removing_garbage([_ | T], Removed) :-
  removing_garbage(T, Removed).

score(Input, Score) :- score(Input, 0, 0, Score).
score([], 0, Accumulator, Accumulator).
score([0'{ | T], Level, Accumulator, Score) :-
  NewLevel is Level + 1,
  score(T, NewLevel, Accumulator, Score).
score([0'} | T], Level, Accumulator, Score) :-
  NewAccumulator is Accumulator + Level,
  NewLevel is Level - 1,
  score(T, NewLevel, NewAccumulator, Score).
score([_ | T], Level, Accumulator, Score) :-
  score(T, Level, Accumulator, Score).
