% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/numbers').

main :-
  current_input(S),
  read_line_to_codes(S, Line),
  number_codes(Input, Line),
  corner(Depth, Corner),
  Input =< Corner,
  !,
  side(Input, Depth, Corner, Side),
  Result is abs(Input - Side) + Depth,
  print(Result).

corner(Depth, Corner) :-
  natural(Depth),
  Corner is (Depth * 2 + 1) ^ 2.

side(0, 1, 1).
side(Input, Depth, Corner, Side) :-
  Depth > 0,
  (
    Input >= Corner - Depth * 2,
    Side is Corner - Depth;
    Input >= Corner - Depth * 4,
    Side is Corner - Depth * 3;
    Input >= Corner - Depth * 6,
    Side is Corner - Depth * 5;
    Side is Corner - Depth * 7
  ).
