use mutable = "collections"
use "collections/persistent"
use "itertools"

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

  fun flippable(): Bool =>
    match operation
    | Nop => true
    | Acc => false
    | Jmp => true
    end

  fun flip(): Instruction =>
    match operation
    | Nop => Instruction(Jmp, argument)
    | Acc => Instruction(Acc, argument)
    | Jmp => Instruction(Nop, argument)
    end

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

class val InstructionPointer is (Equatable[InstructionPointer] & mutable.Hashable & Stringable)
  let _pointer: USize

  new val create(pointer: USize = 0) =>
    _pointer = pointer

  fun lookup(instructions: Instructions): Instruction ? =>
    instructions(_pointer)?

  fun terminates(instructions: Instructions): Bool =>
    _pointer == instructions.size()

  fun advance(increment: ISize): InstructionPointer ? =>
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

  fun string(): String iso^ =>
    _pointer.string()

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
    let flippable = Iter[USize](mutable.Range(0, instructions.size()))
      .filter({ (i) ? => instructions(i)?.flippable() })
      .map[InstructionPointer]({ (i) => InstructionPointer(i) })
    for pointer in flippable do
      _flip_and_solve(instructions, pointer)
    end

  be _flip_and_solve(instructions: Instructions, flip: InstructionPointer) =>
    var accumulator: Accumulator = 0
    var instruction_pointer = InstructionPointer
    var seen: Set[InstructionPointer] = Set[InstructionPointer]
    var success: Bool = false
    try
      while not seen.contains(instruction_pointer) do
        seen = seen + instruction_pointer
        if instruction_pointer.terminates(instructions) then
          success = true
          break
        end
        let instruction =
          if instruction_pointer == flip then
            instruction_pointer.lookup(instructions)?.flip()
          else
            instruction_pointer.lookup(instructions)?
          end
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
      if success then
        _answer.answer(accumulator)
      end
    else
      _answer.fail("Flipping " + flip.string() + " failed.")
    end
