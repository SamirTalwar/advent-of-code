% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').

main :-
  parse_input(Diagram, Start),
  traverse(Diagram, Start, Steps),
  format("~p\n", [Steps]).

traverse(Diagram, Start, Steps) :-
  traverse(Diagram, Start, 0, Steps).

traverse(Diagram, Current, Steps, Output) :-
  X - Y - _ = Current,
  cell_at(X, Y, Diagram, Cell),
  (
    next(Diagram, Cell, Current, Next)
    ->  NewSteps is Steps + 1,
        traverse(Diagram, Next, NewSteps, Output)
    ;   Output = Steps
  ).

next(_, blank, _, _) :-
  false.
next(_, line, X - Y - left, NewX - Y - left) :-
  NewX is X - 1.
next(_, line, X - Y - right, NewX - Y - right) :-
  NewX is X + 1.
next(_, line, X - Y - up, X - NewY - up) :-
  NewY is Y - 1.
next(_, line, X - Y - down, X - NewY - down) :-
  NewY is Y + 1.
next(Diagram, corner, X - Y - left, X - NewY - Direction) :-
  Up is Y - 1,
  Down is Y + 1,
  (
    (cell_at(X, Up, Diagram, Cell), Cell \= blank -> NewY = Up, Direction = up);
    (cell_at(X, Down, Diagram, Cell), Cell \= blank -> NewY = Down, Direction = down)
  ).
next(Diagram, corner, X - Y - right, X - NewY - Direction) :-
  Up is Y - 1,
  Down is Y + 1,
  (
    (cell_at(X, Up, Diagram, Cell), Cell \= blank -> NewY = Up, Direction = up);
    (cell_at(X, Down, Diagram, Cell), Cell \= blank -> NewY = Down, Direction = down)
  ).
next(Diagram, corner, X - Y - up, NewX - Y - Direction) :-
  Left is X - 1,
  Right is X + 1,
  (
    (cell_at(Left, Y, Diagram, Cell), Cell \= blank -> NewX = Left, Direction = left);
    (cell_at(Right, Y, Diagram, Cell), Cell \= blank -> NewX = Right, Direction = right)
  ).
next(Diagram, corner, X - Y - down, NewX - Y - Direction) :-
  Left is X - 1,
  Right is X + 1,
  (
    (cell_at(Left, Y, Diagram, Cell), Cell \= blank -> NewX = Left, Direction = left);
    (cell_at(Right, Y, Diagram, Cell), Cell \= blank -> NewX = Right, Direction = right)
  ).
next(_, letter(_), Position, Next) :-
  next(_, line, Position, Next).

cell_at(X, Y, Diagram, Cell) :-
  nth0(Y, Diagram, Row),
  nth0(X, Row, Cell).

parse_input(Diagram, Start) :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse_line, Lines, Diagram),
  [Top | _] = Diagram,
  nth0(StartX, Top, line),
  Start = StartX - 0 - down,
  !.

parse_line(Line, Row) :-
  maplist(parse_cell, Line, Row).

parse_cell(0' , blank).
parse_cell(0'|, line).
parse_cell(0'-, line).
parse_cell(0'+, corner).
parse_cell(Character, letter(Character)).
