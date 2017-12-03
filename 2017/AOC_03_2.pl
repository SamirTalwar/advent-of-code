% vim: set syntax=prolog

:- dynamic(value/2).

main :-
  current_input(S),
  read_token(S, Input),
  integers(N),
  value(N, Value),
  Value > Input,
  !,
  print(Value).

value(1, 1).
value(N, Value) :-
  N > 1,
  neighbors(N, Neighbors),
  maplist(value, Neighbors, Values),
  sum_list(Values, Value),
  !,
  asserta( (value(N, Value)) ).

neighbors(N, Neighbors) :-
  position(N, X, Y),
  maplist(plus(X), [0, 1, 0, -1, 1, 1, -1, -1], Xs),
  maplist(plus(Y), [-1, 0, 1, 0, -1, 1, -1, 1], Ys),
  maplist(position, Ns, Xs, Ys),
  sort(Ns, PossibleNeighbors),
  filter(valid_neighbor(N), PossibleNeighbors, Neighbors).

valid_neighbor(N, Neighbor) :-
  between(1, N, Neighbor).

position(N, X, Y) :-
  nonvar(N),
  N > 0,
  corner(Depth, Corner),
  N =< Corner,
  !,
  (
    N > Corner - Depth * 2,
    X is Depth - (Corner - N),
    Y is Depth;
    N > Corner - Depth * 4,
    X is -Depth,
    Y is Depth - (Corner - Depth * 2 - N);
    N > Corner - Depth * 6,
    X is -Depth + (Corner - Depth * 4 - N),
    Y is -Depth;
    X is Depth,
    Y is -Depth + (Corner - Depth * 6 - N)
  ),
  !;
  var(N),
  Depth is max(abs(X), abs(Y)),
  corner(Depth, Corner),
  !,
  (
    Y is Depth,
    N is Corner - Depth + X;
    X is -Depth,
    N is Corner - Depth * 3 + Y;
    Y is -Depth,
    N is Corner - Depth * 5 - X;
    X is Depth,
    N is Corner - Depth * 7 - Y
  ),
  !.

corner(Depth, Corner) :-
  integers(Depth),
  Corner is (Depth * 2 + 1) ^ 2.

plus(A, B, C) :-
  C is A + B.
