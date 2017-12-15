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
  maplist(hash, Inputs, Hashes),
  regions(Hashes, Regions),
  length(Regions, Count),
  format("~p\n", [Count]).

row_input(Key, RowNumber, Input) :-
  Infix = "-",
  string_codes(Infix, InfixCodes),
  number_codes(RowNumber, RowString),
  append([Key, InfixCodes, RowString], Input).

regions(Hashes, Regions) :-
  coordinates(Hashes, Locations),
  include(on(Hashes), Locations, OnLocations),
  findall(Location - Region, (
    member(Location, OnLocations),
    region(Hashes, Location, Region)
  ), LocationRegions),
  pairs_keys_values(LocationRegions, _, RegionContents),
  maplist(min_member, UnsortedRegions, RegionContents),
  sort(UnsortedRegions, Regions).

region(Hashes, Location, Region) :- region(Hashes, [Location], [Location], Region).
region(_, [], Region, Region).
region(Hashes, [Location | Queue], Region, Output) :-
  neighbours(Hashes, Location, Neighbours),
  exclude(member_of(Region), Neighbours, UnseenNeighbours),
  append(Queue, UnseenNeighbours, NewQueue),
  append(Region, UnseenNeighbours, NewRegion),
  region(Hashes, NewQueue, NewRegion, Output).

at(Hashes, X - Y, Value) :-
  nth0(Y, Hashes, Row),
  nth0(X, Row, Value).

value_at(Hashes, Value, Location) :-
  at(Hashes, Location, Value).

neighbours(Hashes, X - Y, Neighbours) :-
  XLeft is X - 1,
  XRight is X + 1,
  YUp is Y - 1,
  YDown is Y + 1,
  A = X - YUp,
  B = XRight - Y,
  C = X - YDown,
  D = XLeft - Y,
  AllNeighbours = [A, B, C, D],
  include(value_at(Hashes, 1), AllNeighbours, Neighbours).

on(Hashes, Location) :-
  at(Hashes, Location, 1).

coordinates(Grid, Coordinates) :-
  size(Grid, Rows, Columns),
  naturals(Xs),
  length(Xs, Columns),
  naturals(Ys),
  length(Ys, Rows),
  cartesian_product(Xs, Ys, Coordinates).

size(Grid, Rows, Columns) :-
  [FirstRow | _] = Grid,
  length(Grid, Rows),
  length(FirstRow, Columns).

cartesian_product([], _, []).
cartesian_product([A | As], Bs, Product) :-
  repeated(A, First),
  zip(First, Bs, C),
  cartesian_product(As, Bs, Cs),
  append(C, Cs, Product),
  !.

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
