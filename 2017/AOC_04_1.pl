% vim: set syntax=prolog

main :-
  current_input(S),
  read_lines_of_words(S, Passphrases),
  filter(valid, Passphrases, Valid),
  length(Valid, Count),
  format("~d\n", [Count]).

valid(Words) :-
  forall(select(Word, Words, Rest), \+ member(Word, Rest)).
