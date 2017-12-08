% vim: set syntax=prolog

:- consult('helpers/run').
:- use_module(library(assoc)).
:- use_module('helpers/io').

main :-
  current_input(S),
  read_instructions(S, Instructions),
  empty_assoc(NoRegisters),
  scanl(execute, Instructions, NoRegisters, RegisterIterations),
  max_max_value(RegisterIterations, Value),
  format("~d\n", [Value]).

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

max_max_value(RegisterIterations, Value) :-
  foldl(max_value, RegisterIterations, 0, Value).

max_value(Registers, Value, Max) :-
  assoc_to_values(Registers, Values),
  max_list(Values, CurrentValue)
  ->  Max is max(Value, CurrentValue)
  ;   Max = Value.

invert(Input, Output) :-
  assoc_to_list(Input, Pairs),
  transpose_pairs(Pairs, TransposedPairs),
  ord_list_to_assoc(TransposedPairs, Output).
