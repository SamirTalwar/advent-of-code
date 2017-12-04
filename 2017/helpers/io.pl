% vim: set syntax=prolog

read_digits(S, Ns) :-
  read_line(S, [], Output),
  maplist(code_to_number, Output, Ns).

read_lines_of_words(S, Words) :-
  read_lines(S, Lines),
  maplist(parse_words, Lines, Words).

read_words(S, Words) :-
  read_line(S, Line),
  parse_words(Line, Words).

parse_words(Line, Words) :- parse_words(Line, [], Words).
parse_words([], Output, Words) :- reverse(Output, Words).
parse_words([L | Ls], Output, Words) :-
  take_word([L | Ls], Word, Rest),
  parse_words(Rest, [Word | Output], Words).

take_word(Line, Word, Rest) :- take_word(Line, [], Word, Rest).
take_word([], Output, Word, []) :- reverse(Output, Word).
take_word([C | Cs], Output, Word, Rest) :-
  is_whitespace(C)
  ->  drop_whitespace(Cs, Rest),
      reverse(Output, Word)
  ;   take_word(Cs, [C | Output], Word, Rest).

read_table(S, Table) :-
  read_lines(S, Lines),
  parse_table(Lines, Table).

parse_table(Lines, Table) :-
  maplist(parse_line, Lines, Table).

parse_line(Line, Tokens) :- parse_line(Line, [], Tokens).
parse_line([], Tokens, Result) :-
  reverse(Tokens, Result).
parse_line(Line, Tokens, Result) :-
  read_token_from_codes(Line, Token),
  drop_token(Line, Rest),
  parse_line(Rest, [Token | Tokens], Result).

read_codes(S, Output) :-
  read_line(S, Output).

read_line(S, Output) :- read_line(S, [], Output).
read_line(S, SoFar, Output) :-
    get_code(S, C),
    ( (C == 0'\n ; C == -1)
    ->  reverse(SoFar, Output)
    ;   read_line(S, [C | SoFar], Output)
    ).

read_lines(S, Output) :- read_lines(S, [[]], Output).
read_lines(S, [CurrentLine | Lines], Output) :-
    get_code(S, C),
    ( C == -1
    ->  ( CurrentLine == []
          ->  reverse(Lines, ReversedLines)
          ;   ( reverse(CurrentLine, ReversedCurrentLine),
                reverse([ReversedCurrentLine | Lines], ReversedLines))
        ),
        maplist(reverse, ReversedLines, Output)
    ;   ( C == 0'\n
        ->  read_lines(S, [[] | [CurrentLine | Lines]], Output)
        ;   read_lines(S, [[C | CurrentLine] | Lines], Output)
        )
    ).

code_to_number(C, N) :-
  N is C - 0'0.

drop_token([], []).
drop_token([C | Rest], Output) :-
  is_whitespace(C)
  ->  drop_whitespace(Rest, Output)
  ;   drop_token(Rest, Output).

drop_whitespace([], []).
drop_whitespace([C | Rest], Output) :-
  is_whitespace(C)
  ->  drop_whitespace(Rest, Output)
  ;   Output = [C | Rest].

is_whitespace(0' ).
is_whitespace(0'\t).
is_whitespace(0'\r).
is_whitespace(0'\n).
