% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').
:- use_module('helpers/lists').
:- use_module('helpers/numbers').

main :-
  RowCount = 128,
  current_input(S),
  read_line_to_codes(S, Key),
  repeated(Key, Keys),
  naturals(Rows),
  length(Rows, RowCount),
  maplist(row_input, Keys, Rows, Inputs),
  maplist(count_ones, Inputs, Counts),
  sumlist(Counts, Count),
  format("~p\n", Count).

row_input(Key, RowNumber, Input) :-
  Infix = "-",
  string_codes(Infix, InfixCodes),
  number_codes(RowNumber, RowString),
  append([Key, InfixCodes, RowString], Input).

count_ones(Input, Count) :-
  hash(Input, Hash),
  sumlist(Hash, Count).

hash(Input, Hash) :-
  RoundCount = 64,
  Size = 256,
  ChunkSize = 16,
  Max is Size - 1,
  append(Input, [17, 31, 73, 47, 23], Lengths),
  findall(X, between(0, Max, X), Sequence),
  repeated(Lengths, Rounds),
  length(Rounds, RoundCount),
  append(Rounds, AllLengths),
  sumlist(AllLengths, TotalLength),
  foldl(iterate, AllLengths, 0 - Sequence, MaxSkipSize - RotatedSparseHash),
  TotalSkipSize is MaxSkipSize * (MaxSkipSize - 1) / 2,
  Rotation is -(TotalLength + TotalSkipSize) mod Size,
  rotate(RotatedSparseHash, Rotation, SparseHash),
  chunked(SparseHash, ChunkSize, SparseChunks),
  maplist(dense_hash, SparseChunks, DenseChunks),
  maplist(bits, DenseChunks, NestedBits),
  flatten(NestedBits, Hash),
  !.

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

chunked([], _, []).
chunked(List, ChunkSize, [Chunk | Chunks]) :-
  append(Chunk, Rest, List),
  length(Chunk, ChunkSize),
  chunked(Rest, ChunkSize, Chunks).

dense_hash(Chunk, Hash) :-
  foldl(xor, Chunk, 0, Hash).

xor(A, B, Xored) :- Xored is A xor B.

bits(Number, Bits) :- bits(Number, 128, Bits).
bits(Number, 1, [Number]) :- !.
bits(Number, Divisor, [Bit | Bits]) :-
  divmod(Number, Divisor, Bit, Rest),
  NextDivisor is Divisor div 2,
  bits(Rest, NextDivisor, Bits).

print_hash(Hash) :-
  forall(member(Bit, Hash), (Bit = 1 -> format("#", []) ; format(".", []))),
  format("\n", []).
