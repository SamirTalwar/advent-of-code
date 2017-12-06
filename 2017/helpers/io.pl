% vim: set syntax=prolog

:- module(io_helpers, [
  read_digits/2,
  read_lines/2,
  read_number_table/2,
  read_numbers/2,
  read_word_table/2,
  read_words/2
]).

:- use_module(library(csv)).
:- use_module(library(readutil)).

read_digits(S, Digits) :-
  read_line_to_codes(S, Line),
  maplist(code_to_number, Line, Digits).

read_lines(S, Lines) :- read_lines(S, [], Lines).
read_lines(S, Output, Lines) :-
  read_line_to_codes(S, Line),
  (
    Line = end_of_file
    ->  reverse(Output, Lines)
    ;   read_lines(S, [Line | Output], Lines)
  ).

read_number_table(S, Table) :- read_number_table(S, [], Table).
read_number_table(S, Output, Table) :-
  read_line_to_codes(S, Line),
  (
    Line = end_of_file
    ->  reverse(Output, Table)
    ;   split_string(Line, "\t", "", Cells),
        maplist(number_codes, Numbers, Cells),
        read_number_table(S, [Numbers | Output], Table)
  ).

read_numbers(S, Numbers) :- read_numbers(S, [], Numbers).
read_numbers(S, Output, Numbers) :-
  read_line_to_codes(S, Line),
  (
    Line = end_of_file
    ->  reverse(Output, Numbers)
    ;   number_codes(Number, Line),
        read_numbers(S, [Number | Output], Numbers)
  ).

read_word_table(S, Table) :- read_word_table(S, [], Table).
read_word_table(S, Output, Table) :-
  read_line_to_codes(S, Line),
  (
    Line = end_of_file
    ->  reverse(Output, Table)
    ;   split_string(Line, " ", "", Words),
        read_word_table(S, [Words | Output], Table)
  ).

read_words(S, Words) :-
  read_line_to_codes(S, Line),
  split_string(Line, " ", "", Words).

code_to_number(C, N) :-
  N is C - 0'0.
