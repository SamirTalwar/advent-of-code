% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module('helpers/io').

main :-
  current_input(S),
  read_digits(S, Ns),
  append(As, Bs, Ns),
  length(As, ALength),
  length(Bs, BLength),
  ALength = BLength,
  zip(As, Bs, Pairs),
  matches(Pairs, Matches),
  sum_list(Matches, Sum),
  DoubleSum is Sum * 2,
  format("~d\n", [DoubleSum]).

zip(As, Bs, Pairs) :- zip(As, Bs, [], Pairs).
zip([], _, Result, Result).
zip(_, [], Result, Result).
zip([A | As], [B | Bs], Pairs, Result) :-
  zip(As, Bs, [[A, B] | Pairs], Result).

matches(Pairs, Matches) :- matches(Pairs, [], Matches).
matches([], Result, Result).
matches([[A, A] | Tail], Matches, Result) :-
  matches(Tail, [A | Matches], Result).
matches([[A, B] | Tail], Matches, Result) :-
  A \= B,
  matches(Tail, Matches, Result).
