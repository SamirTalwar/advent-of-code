% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(rbtrees)).
:- use_module('helpers/execution').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  Start = 0 - 0 - up,
  read_input(Input),
  times(10000000, burst, Input - Start - 0, _ - _ - InfectedCount),
  format("~p\n", [InfectedCount]).

burst(Nodes - Position - InfectedCount, NextNodes - NextPosition - NextInfectedCount) :-
  X - Y - _ = Position,
  ( rb_lookup(X - Y, NodeState, Nodes)
    -> !
    ; NodeState = clean),
  manipulate_node(NodeState, NextNodeState),
  ( NextNodeState = clean
    -> rb_delete(Nodes, X - Y, NextNodes)
    ;  rb_insert(Nodes, X - Y, NextNodeState, NextNodes)),
  move(NodeState, Position, NextPosition),
  (
    NextNodeState = infected
    ->  NextInfectedCount is InfectedCount + 1
    ;   NextInfectedCount = InfectedCount
  ).

manipulate_node(clean, weakened).
manipulate_node(weakened, infected).
manipulate_node(infected, flagged).
manipulate_node(flagged, clean).

move(NodeState, Position, NextPosition) :-
  turn(NodeState, Position, Turned),
  move_forward(Turned, NextPosition).

turn(clean, X - Y - up, X - Y - left).
turn(clean, X - Y - left, X - Y - down).
turn(clean, X - Y - down, X - Y - right).
turn(clean, X - Y - right, X - Y - up).

turn(weakened, Position, Position).

turn(infected, X - Y - up, X - Y - right).
turn(infected, X - Y - right, X - Y - down).
turn(infected, X - Y - down, X - Y - left).
turn(infected, X - Y - left, X - Y - up).

turn(flagged, X - Y - up, X - Y - down).
turn(flagged, X - Y - down, X - Y - up).
turn(flagged, X - Y - left, X - Y - right).
turn(flagged, X - Y - right, X - Y - left).

move_forward(X - Y - up,    X - NewY - up)    :- NewY is Y - 1.
move_forward(X - Y - left,  NewX - Y - left)  :- NewX is X - 1.
move_forward(X - Y - down,  X - NewY - down)  :- NewY is Y + 1.
move_forward(X - Y - right, NewX - Y - right) :- NewX is X + 1.

read_input(Input) :-
  current_input(S),
  read_lines(S, Lines),
  length(Lines, Size),
  coordinate(Size, Lines, EnumeratedLines),
  maplist(read_row(Size), EnumeratedLines, Rows),
  flatten(Rows, Cells),
  convlist(input_pairs, Cells, Pairs),
  list_to_rbtree(Pairs, Input).
read_row(Size, Y - Line, Row) :-
  coordinate(Size, Line, Cells),
  maplist(read_cell(Y), Cells, Row).
read_cell(Y, X - Cell, X - Y - Cell).

coordinate(Size, Input, Output) :-
  Max is Size div 2,
  Min is -Max,
  findall(I, between(Min, Max, I), Indices),
  pairs_keys_values(Output, Indices, Input).

input_pairs(X - Y - 0'#, (X - Y) - infected).
