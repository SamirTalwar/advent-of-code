% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(lists)).
:- use_module(library(rbtrees)).
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  current_input(S),
  read_number_line(S, Numbers),
  enumerate(Numbers, Enumerated),
  list_to_rbtree(Enumerated, Banks),
  redistribute_until_cycle(Banks, Count),
  print(Count).

redistribute_until_cycle(Banks, Count) :- redistribute_until_cycle(Banks, 0, [], Count).
redistribute_until_cycle(Banks, Count, Past, Result) :-
  nth1(Distance, Past, Banks)
  -> Result = Distance
  ;
  redistribute(Banks, Distributed),
  NewCount is Count + 1,
  redistribute_until_cycle(Distributed, NewCount, [Banks | Past], Result).

redistribute(Banks, Distributed) :-
  rb_find_max(Banks, Index, Blocks),
  rb_update(Banks, Index, 0, Removed),
  rb_size(Banks, Size),
  Start is (Index + 1) mod Size,
  allocate(Blocks, Start, Removed, Size, Distributed),
  !.

rb_find_max(Tree, Key, Value) :-
  rb_fold(max_pair_by_value, Tree, -1 - 0, Key - Value).
max_pair_by_value(A1 - A2, B1 - B2, C) :-
  A2 > B2
  ->  C = A1-A2
  ;   C = B1-B2.

allocate(0, _, Banks, _, Banks).
allocate(Blocks, Position, Banks, Size, Result) :-
  rb_lookup(Position, BlocksInPosition, Banks),
  NewBlocksInPosition is BlocksInPosition + 1,
  rb_update(Banks, Position, NewBlocksInPosition, NewBanks),
  NewBlocks is Blocks - 1,
  NewPosition is (Position + 1) mod Size,
  allocate(NewBlocks, NewPosition, NewBanks, Size, Result).
