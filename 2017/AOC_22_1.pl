% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(ordsets)).
:- use_module('helpers/execution').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  Start = 0 - 0 - up,
  read_input(Input),
  times(10000, burst, Input - Start - 0, _ - _ - InfectedCount),
  format("~p\n", [InfectedCount]).

burst(InfectedNodes - Position - InfectedCount, NextInfectedNodes - NextPosition - NextInfectedCount) :-
  X - Y - _ = Position,
  (
    ord_memberchk(X - Y, InfectedNodes)
    ->  ord_del_element(InfectedNodes, X - Y, NextInfectedNodes),
        turn_right(Position, Turned),
        move_forward(Turned, NextPosition),
        NextInfectedCount = InfectedCount
    ;   ord_add_element(InfectedNodes, X - Y, NextInfectedNodes),
        turn_left(Position, Turned),
        move_forward(Turned, NextPosition),
        NextInfectedCount is InfectedCount + 1
  ).

turn_right(X - Y - up, X - Y - right).
turn_right(X - Y - right, X - Y - down).
turn_right(X - Y - down, X - Y - left).
turn_right(X - Y - left, X - Y - up).

turn_left(X - Y - up, X - Y - left).
turn_left(X - Y - left, X - Y - down).
turn_left(X - Y - down, X - Y - right).
turn_left(X - Y - right, X - Y - up).

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
  convlist(infected, Cells, Infected),
  sort(Infected, Input).
read_row(Size, Y - Line, Row) :-
  coordinate(Size, Line, Cells),
  maplist(read_cell(Y), Cells, Row).
read_cell(Y, X - Cell, X - Y - Cell).

coordinate(Size, Input, Output) :-
  Max is Size div 2,
  Min is -Max,
  findall(I, between(Min, Max, I), Indices),
  pairs_keys_values(Output, Indices, Input).

infected(X - Y - 0'#, X - Y).
