% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(apply)).
:- use_module('helpers/io').

main :-
  current_input(S),
  read_word_table(S, Passphrases),
  include(valid, Passphrases, Valid),
  length(Valid, Count),
  format("~d\n", [Count]).

valid(Words) :-
  forall(select(Word, Words, Rest), \+ member(Word, Rest)).
