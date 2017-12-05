% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(rbtrees)).
:- use_module('helpers/io').

main :-
  current_input(S),
  read_numbers(S, JumpList),
  convert_list_to_rbtree(JumpList, JumpTree),
  traverse(JumpTree, Count),
  format("~p\n", [Count]).

traverse(Jumps, Count) :-
  Min = 0,
  rb_size(Jumps, Size),
  Max is Size - 1,
  traverse(0, Jumps, Min, Max, 0, Count).
traverse(Index, Jumps, Min, Max, Count, Result) :-
  rb_lookup(Index, Jump, Jumps),
  NewIndex is Index + Jump,
  (
    between(Min, Max, NewIndex)
    ->  ( Jump >= 3
          ->  NewJump is Jump - 1
          ;   NewJump is Jump + 1
        ),
        rb_update(Jumps, Index, NewJump, NewJumps),
        NewCount is Count + 1,
        traverse(NewIndex, NewJumps, Min, Max, NewCount, Result)
    ;   Result is Count + 1
  ).

convert_list_to_rbtree(List, Tree) :-
  rb_empty(Empty),
  convert_list_to_rbtree(List, 0, Empty, Tree).
convert_list_to_rbtree([], _, Result, Result).
convert_list_to_rbtree([H | T], Index, Tree, Result) :-
  rb_insert_new(Tree, Index, H, NewTree),
  NewIndex is Index + 1,
  convert_list_to_rbtree(T, NewIndex, NewTree, Result).
