% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_line_to_codes(S, Line),
  hash(Line, Hash),
  format("~s\n", [Hash]).

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
  foldl(iterate, AllLengths, 0 - Sequence, MaxSkipSize - RotatedSparseHash),
  sumlist(AllLengths, TotalLength),
  TotalSkipSize is MaxSkipSize * (MaxSkipSize - 1) / 2,
  Rotation is -(TotalLength + TotalSkipSize) mod Size,
  rotate(RotatedSparseHash, Rotation, SparseHash),
  chunked(SparseHash, ChunkSize, SparseChunks),
  maplist(dense_hash, SparseChunks, DenseChunks),
  maplist(hex_byte, DenseChunks, NestedHex),
  flatten(NestedHex, Hash).

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

hex_byte(Number, Hex) :-
  divmod(Number, 16, High, Low),
  hex_nibble(High, HighHex),
  hex_nibble(Low, LowHex),
  Hex = [HighHex, LowHex].

hex_nibble( 0, 0'0).
hex_nibble( 1, 0'1).
hex_nibble( 2, 0'2).
hex_nibble( 3, 0'3).
hex_nibble( 4, 0'4).
hex_nibble( 5, 0'5).
hex_nibble( 6, 0'6).
hex_nibble( 7, 0'7).
hex_nibble( 8, 0'8).
hex_nibble( 9, 0'9).
hex_nibble(10, 0'a).
hex_nibble(11, 0'b).
hex_nibble(12, 0'c).
hex_nibble(13, 0'd).
hex_nibble(14, 0'e).
hex_nibble(15, 0'f).
