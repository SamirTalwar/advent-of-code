% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(assoc)).
:- use_module('helpers/io').

main :-
  current_input(S),
  read_instructions(S, Instructions),
  run(Instructions, Registers),
  invert(Registers, RegisterValues),
  max_assoc(RegisterValues, Value, Register),
  format("~s = ~d\n", [Register, Value]).

read_instructions(S, Instructions) :-
  read_word_table(S, Table),
  maplist(parse_instruction, Table, Instructions).

parse_instruction([Register, DirectionString, AmountString, "if", Dependency, DependencyComparisonString, DependencyAmountString], Instruction) :-
  (
    DirectionString = "inc" -> Direction = inc;
    DirectionString = "dec" -> Direction = dec
  ),
  (
    DependencyComparisonString = "==" -> DependencyComparison = =:=;
    DependencyComparisonString = "!=" -> DependencyComparison = =\=;
    DependencyComparisonString = "<"  -> DependencyComparison = <;
    DependencyComparisonString = "<=" -> DependencyComparison = =<;
    DependencyComparisonString = ">"  -> DependencyComparison = >;
    DependencyComparisonString = ">=" -> DependencyComparison = >=
  ),
  number_codes(Amount, AmountString),
  number_codes(DependencyAmount, DependencyAmountString),
  Instruction = (Register - Direction - Amount) - (Dependency - DependencyComparison - DependencyAmount).

run(Instructions, Registers) :-
  empty_assoc(NoRegisters),
  run(Instructions, NoRegisters, Registers).
run([], Output, Output).
run([H | T], Input, Output) :-
  execute(H, Input, Executed),
  run(T, Executed, Output).

execute((Register - Direction - Amount) - (Dependency - DependencyComparison - DependencyAmount), Input, Output) :-
  value(Dependency, Input, DependencyValue),
  call(DependencyComparison, DependencyValue, DependencyAmount)
  ->  value(Register, Input, Value),
      call(Direction, Value, Amount, NewValue),
      put_assoc(Register, Input, NewValue, Output)
  ;   Output = Input.

inc(Value, Amount, NewValue) :-
  NewValue is Value + Amount.

dec(Value, Amount, NewValue) :-
  NewValue is Value - Amount.

value(Register, Registers, Value) :-
  get_assoc(Register, Registers, Value), !;
  Value = 0.

invert(Input, Output) :-
  assoc_to_list(Input, Pairs),
  transpose_pairs(Pairs, TransposedPairs),
  ord_list_to_assoc(TransposedPairs, Output).
