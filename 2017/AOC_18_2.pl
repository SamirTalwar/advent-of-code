% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(assoc)).
:- use_module(library(regex)).
:- use_module('helpers/io').

main :-
  parse(Instructions),
  list_to_assoc([p - 0], Program0_Registers),
  list_to_assoc([p - 1], Program1_Registers),
  Program0 = [] - Instructions - Program0_Registers - [] - [],
  Program1 = [] - Instructions - Program1_Registers - [] - [],
  run(0, Program0, Program1, 0, SentByProgram1),
  format("~p\n", [SentByProgram1]).

run(_, Program0, Program1, Counter, Counter) :-
  forall(member(Program, [Program0, Program1]),
         _ - [command(rcv, _) | _] - _ - _ - [] = Program).
run(0, Program0, Program1, Counter, Output) :-
  execute(0, Program0, Program0Backward - Program0Forward - Program0Registers - Program0Sent - []),
  Program1Backward - Program1Forward - Program1Registers - [] - [] = Program1,
  ExecutedProgram0 = Program0Backward - Program0Forward - Program0Registers - [] - [],
  reverse(Program0Sent, Program1Received),
  PrimedProgram1 = Program1Backward - Program1Forward - Program1Registers - [] - Program1Received,
  run(1, ExecutedProgram0, PrimedProgram1, Counter, Output).
run(1, Program0, Program1, Counter, Output) :-
  execute(1, Program1, Program1Backward - Program1Forward - Program1Registers - Program1Sent - []),
  Program0Backward - Program0Forward - Program0Registers - [] - [] = Program0,
  ExecutedProgram1 = Program1Backward - Program1Forward - Program1Registers - [] - [],
  reverse(Program1Sent, Program0Received),
  PrimedProgram0 = Program0Backward - Program0Forward - Program0Registers - [] - Program0Received,
  length(Program1Sent, Sent),
  Incremented is Counter + Sent,
  run(0, PrimedProgram0, ExecutedProgram1, Incremented, Output).

% execute(CurrentProgram, Backward - [Command | Forward] - Registers - Sent - Received, _) :-
%   length(Backward, BackwardLength),
%   length(Forward, ForwardLength),
%   length(Sent, SentLength),
%   length(Received, ReceivedLength),
%   assoc_to_list(Registers, RegisterList),
%   format("~p:  ~p\t~p\t~p\t~p\t~p\t~p\n", [CurrentProgram, BackwardLength, ForwardLength, SentLength, ReceivedLength, RegisterList, Command]),
%   false.
execute(_, Output, Output) :-
  _ - [command(rcv, _) | _] - _ - _ - [] = Output.
execute(CurrentProgram, Input, Output) :-
  _ - [Current | _] - _ - _ - _ = Input,
  call(Current, Input, Next),
  execute(CurrentProgram, Next, Output).

command(set, register(X), value(Value), Backward - [Current | Forward] - Registers - Sent - Received, [Current | Backward] - Forward - Updated - Sent - Received) :-
  put_assoc(X, Registers, Value, Updated).
command(add, register(X), value(Value), Backward - [Current | Forward] - Registers - Sent - Received, [Current | Backward] - Forward - Updated - Sent - Received) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue + Value,
  put_assoc(X, Registers, NewValue, Updated).
command(mul, register(X), value(Value), Backward - [Current | Forward] - Registers - Sent - Received, [Current | Backward] - Forward - Updated - Sent - Received) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue * Value,
  put_assoc(X, Registers, NewValue, Updated).
command(mod, register(X), value(Value), Backward - [Current | Forward] - Registers - Sent - Received, [Current | Backward] - Forward - Updated - Sent - Received) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue mod Value,
  put_assoc(X, Registers, NewValue, Updated).
command(Command, X, register(Register), Input, Output) :-
  _ - _ - Registers - _ - _ = Input,
  value_of(Register, Registers, Value),
  command(Command, X, value(Value), Input, Output).

command(jgz, X, Y, Backward - Forward - Registers - Sent - Received, NewBackward - NewForward - Registers - Sent - Received) :-
  [command(jgz, X, Y) | _] = Forward,
  (X = value(XValue) ; X = register(XRegister), value_of(XRegister, Registers, XValue)),
  (Y = value(YValue) ; Y = register(YRegister), value_of(YRegister, Registers, YValue)),
  !,
  (
    XValue =< 0
    ->  [Current | NewForward] = Forward,
        NewBackward = [Current | Backward]
    ;   JumpDistance is abs(YValue),
        ( YValue > 0
          ->  length(Forward, ForwardLength),
              ( ForwardLength < JumpDistance
                ->  NewBackward = Backward,
                    NewForward = Forward
                ;   length(Jump, JumpDistance),
                    append(Jump, NewForward, Forward),
                    reverse(Jump, ReverseJump),
                    append(ReverseJump, Backward, NewBackward)
              )
          ; ( YValue < 0
              ->  length(Backward, BackwardLength),
                  ( BackwardLength < JumpDistance
                    ->  NewBackward = Backward,
                        NewForward = Forward
                    ;   length(Jump, JumpDistance),
                        append(Jump, NewBackward, Backward),
                        reverse(Jump, ReverseJump),
                        append(ReverseJump, Forward, NewForward)
                  )
            )
        )
  ),
  !.

command(snd, value(Value), Backward - [Current | Forward] - Registers - Sent - Received, [Current | Backward] - Forward - Registers - [Value | Sent] - Received).
command(snd, register(X), Backward - [Current | Forward] - Registers - Sent - Received, [Current | Backward] - Forward - Registers - [Value | Sent] - Received) :-
  value_of(X, Registers, Value).
command(rcv, register(X), Backward - [Current | Forward] - Registers - Sent - [Value | Received], [Current | Backward] - Forward - Updated - Sent - Received) :-
  put_assoc(X, Registers, Value, Updated).

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
