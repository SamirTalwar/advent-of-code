right(north, east).
right(east, south).
right(south, west).
right(west, north).

left(north, west).
left(west, south).
left(south, east).
left(east, north).

forward(N, (X, Y, north), (X, Z, north)) :- Z is Y + N.
forward(N, (X, Y, east), (Z, Y, east)) :- Z is X + N.
forward(N, (X, Y, south), (X, Z, south)) :- Z is Y - N.
forward(N, (X, Y, west), (Z, Y, west)) :- Z is X - N.

move([], Vector, Vector).
move([right | Tail], (X, Y, Start), Destination)
  :- right(Start, Finish),
     move(Tail, (X, Y, Finish), Destination).
move([left | Tail], (X, Y, Start), Destination)
  :- left(Start, Finish),
     move(Tail, (X, Y, Finish), Destination).
move([forward(N) | Tail], Start, Destination)
  :- forward(N, Start, Finish),
     move(Tail, Finish, Destination).

blocks((X, Y, _), N) :- abs(X, XA), abs(Y, YA), N is XA + YA.
