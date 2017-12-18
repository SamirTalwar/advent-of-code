% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(assoc)).
:- use_module(library(regex)).
:- use_module('helpers/io').

main :-
  parse(Instructions),
  empty_assoc(InitialRegisters),
  execute([] - Instructions - InitialRegisters - [] - [], _ - _ - _ - _ - Recovered),
  format("~p\n", [Recovered]).

% execute(Backward - [Command | Forward] - Registers - _ - _, _) :-
%   length(Backward, BackwardLength),
%   length(Forward, ForwardLength),
%   assoc_to_list(Registers, RegisterList),
%   format("~p\t~p\t~p\t~p\n", [BackwardLength, ForwardLength, RegisterList, Command]),
%   false.
execute(Input, Output) :-
  _ - [Current | _] - Registers - _ - _ = Input,
  (
    Current = command(rcv, X)
  ->  (X = value(Value) ; X = register(XRegister), value_of(XRegister, Registers, Value)),
      (Value \= 0 -> call(Current, Input, Output))
  ;   call(Current, Input, Next),
      execute(Next, Output)
  ).

command(set, register(X), value(Value), Backward - [Current | Forward] - Registers - Played - Recovered, [Current | Backward] - Forward - Updated - Played - Recovered) :-
  put_assoc(X, Registers, Value, Updated).
command(add, register(X), value(Value), Backward - [Current | Forward] - Registers - Played - Recovered, [Current | Backward] - Forward - Updated - Played - Recovered) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue + Value,
  put_assoc(X, Registers, NewValue, Updated).
command(mul, register(X), value(Value), Backward - [Current | Forward] - Registers - Played - Recovered, [Current | Backward] - Forward - Updated - Played - Recovered) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue * Value,
  put_assoc(X, Registers, NewValue, Updated).
command(mod, register(X), value(Value), Backward - [Current | Forward] - Registers - Played - Recovered, [Current | Backward] - Forward - Updated - Played - Recovered) :-
  value_of(X, Registers, CurrentValue),
  NewValue is CurrentValue mod Value,
  put_assoc(X, Registers, NewValue, Updated).
command(Command, X, register(Register), Input, Output) :-
  _ - _ - Registers - _ - _ = Input,
  value_of(Register, Registers, Value),
  command(Command, X, value(Value), Input, Output).

command(jgz, X, Y, Backward - Forward - Registers - Played - Recovered, NewBackward - NewForward - Registers - Played - Recovered) :-
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

command(snd, register(X), Backward - [Current | Forward] - Registers - Played - Recovered, [Current | Backward] - Forward - Registers - [Value | Played] - Recovered) :-
  value_of(X, Registers, Value).
command(rcv, register(X), Backward - [Current | Forward] - Registers - Played - Recovered, [Current | Backward] - Forward - Registers - Played - Updated) :-
  value_of(X, Registers, Value),
  (
    Value \= 0
    ->  [LastPlayed | _] = Played,
        Updated = [LastPlayed | Recovered]
    ;   Updated = Recovered
  ).

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
