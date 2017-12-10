% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').

main :-
  Size = 256,
  Max is Size - 1,
  current_input(S),
  read_number_line(S, ",", Lengths),
  findall(X, between(0, Max, X), Sequence),
  foldl(iterate, Lengths, 0 - Sequence, MaxSkipSize - RotatedResult),
  sumlist(Lengths, TotalLength),
  TotalSkipSize is MaxSkipSize * (MaxSkipSize - 1) / 2,
  Rotation is -(TotalLength + TotalSkipSize) mod Size,
  rotate(RotatedResult, Rotation, Result),
  [A, B | _] = Result,
  Answer is A * B,
  format("~d * ~d = ~d\n", [A, B, Answer]).

iterate(Length, SkipSize - Sequence, NewSkipSize - NewSequence) :-
  append(Picked, Suffix, Sequence),
  length(Picked, Length),
  reverse(Picked, Reversed),
  append(Reversed, Suffix, SequenceReversed),
  Rotation is Length + SkipSize,
  rotate(SequenceReversed, Rotation, NewSequence),
  NewSkipSize is SkipSize + 1.

rotate(Input, Rotation, Output) :-
  length(Input, Length),
  ActualRotation is Rotation mod Length,
  append(A, B, Input),
  length(A, ActualRotation),
  append(B, A, Output).
