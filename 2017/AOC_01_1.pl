% vim: set syntax=prolog

main :-
  current_input(S),
  read_numbers(S, Ns),
  [Head | _] = Ns,
  append(Ns, [Head], CycledNs),
  matches_next(CycledNs, Matches),
  sum(Matches, Sum),
  format("~d\n", [Sum]).

matches_next(Ns, Matches) :- matches_next(Ns, [], Matches).
matches_next([], Matches, Matches).
matches_next([_], Matches, Matches).
matches_next([A, A | Rest], Matches, Output) :-
  matches_next([A | Rest], [A | Matches], Output).
matches_next([_, B | Rest], Matches, Output) :-
  matches_next([B | Rest], Matches, Output).

sum(Values, Result) :- sum(Values, 0, Result).
sum([], Result, Result).
sum([V | Vs], Accumulator, Result) :-
  Next is Accumulator + V,
  sum(Vs, Next, Result).