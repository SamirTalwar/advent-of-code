% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(assoc)).
:- use_module(library(regex)).
:- use_module('helpers/execution').

main :-
  read_input(BeginningState - DiagnosticChecksumSteps - Instructions),
  times(DiagnosticChecksumSteps, run(Instructions), BeginningState - [] - 0 - [], _ - Left - Current - Right),
  append([Left, [Current], Right], Tape),
  sumlist(Tape, Ones),
  format("~p\n", [Ones]).

run(Instructions, State - Left - Current - Right, NextState - NextLeft - NextCurrent - NextRight) :-
  get_assoc(State, Instructions, StateInstructions),
  nth0(Current, StateInstructions, NextValue - Move - NextState),
  move(Move, Left, NextValue, Right, NextLeft, NextCurrent, NextRight).

move(left, [], Current, Right, [], 0, [Current | Right]).
move(left, [NextCurrent | NextLeft], Current, Right, NextLeft, NextCurrent, [Current | Right]).
move(right, Left, Current, [], [Current | Left], 0, []).
move(right, Left, Current, [NextCurrent | NextRight], [Current | Left], NextCurrent, NextRight).

read_input(BeginningState - DiagnosticChecksumSteps - Instructions) :-
  current_input(S),
  read_beginning_state(S, BeginningState),
  read_steps_for_diagnostic_checksum(S, DiagnosticChecksumSteps),
  read_instructions(S, Instructions).

read_beginning_state(S, State) :-
  read_line_to_codes(S, Line),
  regex("^Begin in state ([A-Z]+)\\.$", [], Line, [StateCodes]),
  atom_codes(State, StateCodes).

read_steps_for_diagnostic_checksum(S, DiagnosticChecksumSteps) :-
  read_line_to_codes(S, Line),
  regex("^Perform a diagnostic checksum after (\\d+) steps\\.$", [], Line, [StepsCodes]),
  number_codes(DiagnosticChecksumSteps, StepsCodes).

read_instructions(S, Instructions) :-
  read_instructions(S, [], InstructionList),
  list_to_assoc(InstructionList, Instructions).
read_instructions(S, Instructions, Output) :-
  read_line_to_codes(S, Check),
  (
    Check = end_of_file
    ->  Instructions = Output
    ;   read_state_instructions(S, StateInstructions),
        read_instructions(S, [StateInstructions | Instructions], Output)
  ).

read_state_instructions(S, State - [InstructionsFor0, InstructionsFor1]) :-
  read_line_to_codes(S, StateLine),
  regex("^In state ([A-Z]+):$", [], StateLine, [StateCodes]),
  atom_codes(State, StateCodes),
  read_line_to_codes(S, _),
  read_value_instructions(S, InstructionsFor0),
  read_line_to_codes(S, _),
  read_value_instructions(S, InstructionsFor1).

read_value_instructions(S, Write - Move - NewState) :-
  read_line_to_codes(S, WriteLine),
  regex("^    - Write the value (\\d+).$", [], WriteLine, [WriteCodes]),
  number_codes(Write, WriteCodes),
  read_line_to_codes(S, MoveLine),
  regex("^    - Move one slot to the ([a-z]+).$", [], MoveLine, [MoveCodes]),
  atom_codes(Move, MoveCodes),
  read_line_to_codes(S, NewStateLine),
  regex("^    - Continue with state ([A-Z]+).$", [], NewStateLine, [NewStateCodes]),
  atom_codes(NewState, NewStateCodes).
