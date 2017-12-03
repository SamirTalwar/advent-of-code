% vim: set syntax=prolog

main :-
  current_input(S),
  read_numbers(S, Ns),
  print(Ns),
  format("\n", []).
