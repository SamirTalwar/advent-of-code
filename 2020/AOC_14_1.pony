use collections = "collections"
use "itertools"
use "regex"

class val Mask
  let zeroes: USize
  let ones: USize

  new val create(zeroes': USize, ones': USize) =>
    zeroes = zeroes'
    ones = ones'

class val Write
  let address: USize
  let value: USize

  new val create(address': USize, value': USize) =>
    address = address'
    value = value'

type Program is Array[(Mask | Write)] val

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    try
      let collector = LineCollector[(Mask | Write)](orchestrator, Parser.create()?)
      let solution = Solution(orchestrator)
      orchestrator.start[Program](collector, solution)
    else
      orchestrator.fail("Could not construct the parser.")
    end

class Parser is SingleItemParser[(Mask | Write)]
  let _mask_parser: Regex val = recover val Regex("^mask = ([01X]+)$")? end
  let _mem_parser: Regex val = recover val Regex("^mem\\[(\\d+)\\] = (\\d+)$")? end

  new iso create() ? =>
    None

  fun parse(line: String val): (Mask | Write) ? =>
    try
      let matched = recover val _mask_parser(line)? end
      let mask = recover val matched(1)? end
      let zeroes = Iter[U8](mask.array().values()).fold_partial[USize](0, { (acc, ch) ? =>
        match ch
        | '0' => (acc << 1)
        | '1' => (acc << 1) or 1
        | 'X' => (acc << 1) or 1
        else error
        end
      })?
      let ones = Iter[U8](mask.array().values()).fold_partial[USize](0, { (acc, ch) ? =>
        match ch
        | '0' => (acc << 1)
        | '1' => (acc << 1) or 1
        | 'X' => (acc << 1)
        else error
        end
      })?
      Mask(zeroes, ones)
    else
      let matched = recover val _mem_parser(line)? end
      let address = matched(1)?.usize()?
      let value = matched(2)?.usize()?
      Write(address, value)
    end

actor Solution is Solve[Program]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(program: Program) =>
    var current_mask: (Mask | None) = None
    let memory = collections.Map[USize, USize]
    for instruction in program.values() do
      match instruction
      | let mask: Mask =>
        current_mask = mask
      | let write: Write =>
        match current_mask
        | None =>
          _answer.fail("No mask provided.")
          return
        | let mask: Mask =>
          memory(write.address) = (write.value and mask.zeroes) or mask.ones
        end
      end
    end
    let result = Iter[USize](memory.values()).fold[USize](0, { (sum, value) => sum + value })
    _answer.answer(result)
