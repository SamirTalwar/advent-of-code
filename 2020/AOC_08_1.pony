use mutable = "collections"
use "collections/persistent"

primitive Nop
primitive Acc
primitive Jmp
type Operation is (Nop | Acc | Jmp)
type Argument is ISize
type Accumulator is ISize

class val Instruction
  let operation: Operation
  let argument: Argument

  new val create(operation': Operation, argument': Argument) =>
    operation = operation'
    argument = argument'

  fun string(): String =>
    let operation_string = match operation
    | Nop => "nop"
    | Acc => "acc"
    | Jmp => "jmp"
    end
    let argument_string =
      if argument < 0 then
        argument.string()
      else
        "+" + argument.string()
      end
    operation_string + " " + argument_string

type Instructions is Array[Instruction] val

class val InstructionPointer is (Equatable[InstructionPointer] & mutable.Hashable)
  let _pointer: USize

  new val create(pointer: USize = 0) =>
    _pointer = pointer

  fun box lookup(instructions: Instructions): Instruction ? =>
    instructions(_pointer)?

  fun box advance(increment: ISize): InstructionPointer ? =>
    let new_pointer = _pointer.isize() +? increment
    if new_pointer < 0 then
      error
    else
      InstructionPointer(new_pointer.usize())
    end

  fun eq(that: box->InstructionPointer): Bool =>
    this._pointer == that._pointer

  fun hash(): USize =>
    _pointer.hash()

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = LineCollector[Instruction](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[Instruction] val](collector, solution)

class Parser is SingleItemParser[Instruction]
  fun parse(line: String): Instruction ? =>
    let split = line.split_by(" ")
    if split.size() == 2 then
      match (split(0)?, split(1)?)
      | ("nop", let argument: String) => Instruction(Nop, _parse_argument(argument)?)
      | ("acc", let argument: String) => Instruction(Acc, _parse_argument(argument)?)
      | ("jmp", let argument: String) => Instruction(Jmp, _parse_argument(argument)?)
      else error
      end
    else
      error
    end

  fun _parse_argument(string: String): Argument ? =>
    match string.substring(0, 1)
    | "+" => string.substring(1).isize()?
    | "-" => string.isize()?
    else error
    end

actor Solution is Solve[Instructions]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(instructions: Instructions) =>
    var accumulator: Accumulator = 0
    var instruction_pointer = InstructionPointer
    var seen: Set[InstructionPointer] = Set[InstructionPointer]
    try
      while not seen.contains(instruction_pointer) do
        seen = seen + instruction_pointer
        let instruction = instruction_pointer.lookup(instructions)?
        match instruction.operation
        | Nop =>
          instruction_pointer = instruction_pointer.advance(1)?
        | Acc =>
          accumulator = accumulator + instruction.argument
          instruction_pointer = instruction_pointer.advance(1)?
        | Jmp =>
          instruction_pointer = instruction_pointer.advance(instruction.argument)?
        end
      end
      _answer.answer(accumulator)
    else
      _answer.fail("Didn't work.")
    end
