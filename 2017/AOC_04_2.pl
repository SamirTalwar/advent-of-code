% vim: set syntax=prolog

main :-
  current_input(S),
  read_lines_of_words(S, Passphrases),
  filter(valid, Passphrases, Valid),
  length(Valid, Count),
  format("~d\n", [Count]).

valid(Words) :-
  forall(pick(Words, A, B), \+ permutation(A, B)).

pick(List, A, B) :-
  select(A, List, Rest),
  member(B, Rest).
