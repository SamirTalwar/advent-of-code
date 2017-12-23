% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(assoc)).
:- use_module(library(regex)).
:- use_module('helpers/io').

main :-
  parse(Instructions),
  empty_assoc(InitialRegisters),
  execute([] - Instructions - InitialRegisters - 0, _ - _ - _ - MulCount),
  format("~p\n", [MulCount]).

% execute(Backward - [Command | Forward] - Registers - _, _) :-
%   length(Backward, BackwardLength),
%   length(Forward, ForwardLength),
%   assoc_to_list(Registers, RegisterList),
%   format("~p\t~p\t~p\t~p\n", [BackwardLength, ForwardLength, RegisterList, Command]),
%   false.
execute(Result, Result) :-
  _ - [] - _ - _ = Result.
execute(Input, Output) :-
  _ - [Current | _] - _ - _ = Input,
  call(Current, Input, Next, Exit),
  (
    call(Exit)
    ->  Output = Next
    ;   execute(Next, Output)
  ).

command(set, register(X), value(Value), Backward - [Current | Forward] - Registers - MulCount,
                                        [Current | Backward] - Forward - Updated - MulCount,
                                        false) :-
  put_assoc(X, Registers, Value, Updated).
command(sub, register(X), value(Value), Backward - [Current | Forward] - Registers - MulCount,
                                        [Current | Backward] - Forward - Updated - MulCount,
                                        false) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue - Value,
  put_assoc(X, Registers, NewValue, Updated).
command(mul, register(X), value(Value), Backward - [Current | Forward] - Registers - MulCount,
                                        [Current | Backward] - Forward - Updated - NewMulCount,
                                        false) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue * Value,
  NewMulCount is MulCount + 1,
  put_assoc(X, Registers, NewValue, Updated).
command(Command, X, register(Register), Input, Output, Exit) :-
  _ - _ - Registers - _ = Input,
  value_of(Register, Registers, Value),
  command(Command, X, value(Value), Input, Output, Exit).

command(jnz, X, Y, Backward - Forward - Registers - MulCount,
                   NewBackward - NewForward - Registers - MulCount,
                   Exit) :-
  (X = value(XValue) ; X = register(XRegister), value_of(XRegister, Registers, XValue)),
  (Y = value(YValue) ; Y = register(YRegister), value_of(YRegister, Registers, YValue)),
  !,
  (
    XValue = 0
    ->  [Current | NewForward] = Forward,
        NewBackward = [Current | Backward],
        length(NewForward, Remaining),
        Exit = (Remaining = 0)
    ;   JumpDistance is abs(YValue),
        ( YValue > 0
          ->  length(Forward, ForwardLength),
              ( ForwardLength < JumpDistance
                ->  NewBackward = Backward,
                    NewForward = Forward,
                    Exit = true
                ;   length(Jump, JumpDistance),
                    append(Jump, NewForward, Forward),
                    reverse(Jump, ReverseJump),
                    append(ReverseJump, Backward, NewBackward),
                    Exit = false
              )
          ; ( YValue < 0
              ->  length(Backward, BackwardLength),
                  ( BackwardLength < JumpDistance
                    ->  NewBackward = Backward,
                        NewForward = Forward,
                        Exit = true
                    ;   length(Jump, JumpDistance),
                        append(Jump, NewBackward, Backward),
                        reverse(Jump, ReverseJump),
                        append(ReverseJump, Forward, NewForward),
                        Exit = false
                  )
            )
        )
  ),
  !.

value_of(Register, Registers, Value) :-
  get_assoc(Register, Registers, Value),
  !.
value_of(_, _, 0).

parse(Instructions) :-
  current_input(S),
  read_lines(S, Lines),
  maplist(parse_instruction, Lines, Instructions).

parse_instruction(Line, command(Command, X)) :-
  regex("^(\\w+) (\\w+)$", [], Line, Captures),
  [CommandString, XString] = Captures,
  atom_codes(Command, CommandString),
  parse_argument(XString, X),
  !.
parse_instruction(Line, command(Command, X, Y)) :-
  regex("^(\\w+) (-?\\w+) (-?\\w+)$", [], Line, Captures),
  [CommandString, XString, YString] = Captures,
  atom_codes(Command, CommandString),
  parse_argument(XString, X),
  parse_argument(YString, Y),
  !.
parse_instruction(Line, _) :-
  format("Could not parse the following instruction:\n~s\n", [Line]),
  false.

parse_argument(String, value(Value)) :-
  regex("^-?\\d+$", [], String, _),
  number_codes(Value, String).
parse_argument(String, register(Register)) :-
  atom_codes(Register, String).
