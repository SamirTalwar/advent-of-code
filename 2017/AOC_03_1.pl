% vim: set syntax=prolog

main :-
  current_input(S),
  read_token(S, Input),
  corner(Depth, Corner),
  Input =< Corner,
  !,
  side(Input, Depth, Corner, Side),
  Result is abs(Input - Side) + Depth,
  print(Result).

corner(Depth, Corner) :-
  numbers(Depth),
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

numbers(0).
numbers(X) :-
  numbers(Y),
  X is Y + 1.
