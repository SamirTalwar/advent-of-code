% vim: set syntax=prolog

:- consult('helpers/run').

main :-
  current_input(S),
  read_line_to_codes(S, Input),
  count_garbage(Input, Count),
  format("~d\n", [Count]).

count_garbage(Input, Count) :- count_garbage(Input, 0, Count).
count_garbage([], Accumulator, Accumulator).
count_garbage([0'< | T], Accumulator, Count) :-
  removing_garbage(T, RemovedCount, Removed),
  NewAccumulator is Accumulator + RemovedCount,
  count_garbage(Removed, NewAccumulator, Count).
count_garbage([_ | T], Accumulator, Count) :-
  count_garbage(T, Accumulator, Count).

removing_garbage(Input, RemovedCount, Removed) :-
  removing_garbage(Input, 0, RemovedCount, Removed).
removing_garbage([0'!, _ | T], Counter, RemovedCount, Removed) :-
  removing_garbage(T, Counter, RemovedCount, Removed).
removing_garbage([0'> | T], Counter, Counter, T).
removing_garbage([_ | T], Counter, RemovedCount, Removed) :-
  NewCounter is Counter + 1,
  removing_garbage(T, NewCounter, RemovedCount, Removed).
