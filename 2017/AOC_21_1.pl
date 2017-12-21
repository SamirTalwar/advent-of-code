% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(assoc)).
:- use_module('helpers/execution').
:- use_module('helpers/io').
:- use_module('helpers/lists').

main :-
  StartStrings = [".#.", "..#", "###"],
  maplist(string_codes, StartStrings, Start),
  read_input(Rules),
  times(5, iterate(Rules), Start, Result),
  show(Result),
  count_on(Result, On),
  format("\nOn: ~p\n", [On]).

iterate(Rules, Input, Output) :-
  split(Input, Sections),
  enhance_sections(Rules, Sections, Stepped),
  join(Stepped, Output).

split(Grid, Sections) :-
  length(Grid, Size),
  DivisibleBy2 is Size mod 2,
  (
    DivisibleBy2 = 0
    ->  split_rows(2, Grid, Sections)
    ;   split_rows(3, Grid, Sections)
  ).
split_rows(_, [], []) :- !.
split_rows(N, Rows, [SectionRow | Sections]) :-
  split_at(N, Rows, CellRows, Rest),
  split_cells(N, CellRows, SectionRow),
  split_rows(N, Rest, Sections).
split_cells(_, Rows, []) :-
  forall(member(Row, Rows), Row = []),
  !.
split_cells(N, Rows, [SectionCell | SectionRow]) :-
  maplist(split_at(N), Rows, SectionCell, Rest),
  split_cells(N, Rest, SectionRow).

join([], []).
join([SectionRow | Sections], Grid) :-
  join_rows(SectionRow, Rows),
  join(Sections, Rest),
  append(Rows, Rest, Grid).
join_rows(Rows, []) :-
  forall(member(Row, Rows), Row = []),
  !.
join_rows(Sections, [Row | Grid]) :-
  maplist(split_at(1), Sections, NestedCells, Rest),
  maplist(append, NestedCells, Cells),
  append(Cells, Row),
  join_rows(Rest, Grid).

enhance_sections(Rules, Input, Output) :-
  maplist(enhance_rows(Rules), Input, Output).
enhance_rows(Rules, Input, Output) :-
  maplist(enhance(Rules), Input, Output).

enhance(Rules, Input, Output) :-
  get_assoc(Input, Rules, Output).

count_on(Grid, On) :-
  maplist(count_on_in_rows, Grid, OnInRows),
  sumlist(OnInRows, On).
count_on_in_rows(Row, On) :-
  include(=(0'#), Row, CellsOn),
  length(CellsOn, On).

read_input(Rules) :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse_rule, Lines, NestedRuleList),
  flatten(NestedRuleList, RuleList),
  list_to_assoc(RuleList, Rules).

parse_rule(Line, Rules) :-
  string_codes(" => ", Arrow),
  append([JoinedFrom, Arrow, JoinedTo], Line),
  split_string(JoinedFrom, "/", "", FromString),
  split_string(JoinedTo, "/", "", ToString),
  maplist(string_codes, FromString, From),
  maplist(string_codes, ToString, To),
  patterns(From, Froms),
  repeated(To, Tos),
  zip(Froms, Tos, Rules).

patterns(Input, Manipulated) :-
  rotations(Input, Rotations),
  maplist(flipped, Rotations, Flipped),
  append(Flipped, Unsorted),
  sort(Unsorted, Manipulated).

flipped(Grid, [Grid, FlippedX, FlippedY]) :-
  flipX(Grid, FlippedX),
  flipY(Grid, FlippedY).

flipX(Grid, Flipped) :-
  maplist(reverse, Grid, Flipped).

flipY(Grid, Flipped) :-
  reverse(Grid, Flipped).

rotations(Grid, [Grid, RotatedOnce, RotatedTwice, RotatedThrice]) :-
  rotate(Grid, RotatedOnce),
  rotate(RotatedOnce, RotatedTwice),
  rotate(RotatedTwice, RotatedThrice).

rotate([[A1, A2], [B1, B2]],
       [[B1, A1], [B2, A2]]).
rotate([[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]],
       [[C1, B1, A1], [C2, B2, A2], [C3, B3, A3]]).

show(Grid) :-
  format("\n"),
  maplist(show_row, Grid).

show_row(Row) :-
  format("~s\n", [Row]).
