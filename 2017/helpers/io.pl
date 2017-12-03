% vim: set syntax=prolog

read_digits(S, Ns) :-
  read_line(S, [], Cs),
  maplist(code_to_number, Cs, Ns).

read_codes(S, Cs) :-
  read_line(S, [], Cs).

read_line(S, SoFar, Cs) :-
    get_code(S, C),
    (   (C == 0'\n; C == -1)
    ->  reverse(SoFar, Cs)
    ;   read_line(S, [C|SoFar], Cs)
    ).

code_to_number(C, N) :-
  N is C - 0'0.
