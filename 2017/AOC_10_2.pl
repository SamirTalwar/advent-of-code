% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  RoundCount = 64,
  Size = 256,
  ChunkSize = 16,
  Max is Size - 1,
  current_input(S),
  read_line_to_codes(S, Line),
  append(Line, [17, 31, 73, 47, 23], Lengths),
  findall(X, between(0, Max, X), Sequence),
  repeated(Lengths, Rounds),
  length(Rounds, RoundCount),
  append(Rounds, AllLengths),
  foldl(iterate, AllLengths, 0 - Sequence, MaxSkipSize - RotatedSparseHash),
  sumlist(AllLengths, TotalLength),
  TotalSkipSize is MaxSkipSize * (MaxSkipSize - 1) / 2,
  Rotation is -(TotalLength + TotalSkipSize) mod Size,
  rotate(RotatedSparseHash, Rotation, SparseHash),
  chunked(SparseHash, ChunkSize, SparseChunks),
  maplist(dense_hash, SparseChunks, DenseChunks),
  forall(member(DenseChunk, DenseChunks), print_hex_byte(DenseChunk)).

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

print_hex_byte(X) :-
  X >= 16
  ->  format('~16r', [X])
  ;   format('0~16r', [X]).
