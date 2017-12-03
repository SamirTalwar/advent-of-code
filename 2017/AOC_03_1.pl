% vim: set syntax=prolog

main :-
  current_input(S),
  read_token(S, Input),
  corners(Corners),
  member([Depth, Corner], Corners),
  Input =< Corner,
  !,
  side(Input, Depth, Corner, Side),
  Result is abs(Input - Side) + Depth,
  print(Result).

corners(Output) :- corners(0, [], Output).
corners(Current, Accumulator, Output) :-
  Current >= 500
  ->  reverse(Accumulator, Output)
  ;   Next is Current + 1,
      Corner is (Current * 2 + 1) ^ 2,
      corners(Next, [[Current, Corner] | Accumulator], Output).

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
