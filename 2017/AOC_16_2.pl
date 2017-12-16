% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(regex)).
:- use_module('helpers/execution').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  ProgramCount = 16,
  Performances = 1000000000,
  programs(ProgramCount, Programs),
  parse_input(DanceMoves),
  times_with_repetition(Performances, foldl(call, DanceMoves), Programs, Result),
  maplist(atom_codes, Result, ResultStrings),
  append(ResultStrings, ResultString),
  format("~s\n", [ResultString]).

spin(Amount, Input, Output) :-
  length(B, Amount),
  append(A, B, Input),
  append(B, A, Output),
  !.

exchange(APosition, BPosition, Input, Output) :-
  APosition < BPosition,
  length(Prefix, APosition),
  InfixLength is BPosition - APosition - 1,
  length(Infix, InfixLength),
  append([Prefix, [A], Infix, [B], Suffix], Input),
  append([Prefix, [B], Infix, [A], Suffix], Output),
  !.
exchange(APosition, BPosition, Input, Output) :-
  APosition > BPosition,
  exchange(BPosition, APosition, Input, Output).

partner(A, B, Input, Output) :-
  append([Prefix, [A], Infix, [B], Suffix], Input),
  append([Prefix, [B], Infix, [A], Suffix], Output),
  !.
partner(A, B, Input, Output) :-
  append([Prefix, [B], Infix, [A], Suffix], Input),
  append([Prefix, [A], Infix, [B], Suffix], Output),
  !.

programs(ProgramCount, Programs) :-
  MaxProgramCode is 0'a + ProgramCount - 1,
  findall(ProgramCode, between(0'a, MaxProgramCode, ProgramCode), ProgramCodes),
  maplist(atom_char, Programs, ProgramCodes).

parse_input(DanceMoves) :-
  current_input(S),
  read_words(S, ",", Words),
  maplist(string_codes, Words, WordCodes),
  maplist(parse, WordCodes, DanceMoves).

parse([0's | AmountString], spin(Amount)) :-
  number_codes(Amount, AmountString),
  !.

parse([0'x | Args], exchange(A, B)) :-
  split_string(Args, "/", "", [AString, BString]),
  number_codes(A, AString),
  number_codes(B, BString),
  !.

parse([0'p, ACode, 0'/, BCode], partner(A, B)) :-
  atom_char(A, ACode),
  atom_char(B, BCode),
  !.

parse(Word, _) :-
  format("Could not parse \"~s\".\n", [Word]),
  false.

atom_char(Atom, Char) :-
  atom_codes(Atom, [Char]).
