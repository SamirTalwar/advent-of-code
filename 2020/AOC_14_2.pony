use collections = "collections"
use "itertools"
use "regex"

primitive Zero
primitive One
primitive Floating
type MaskBit is (Zero | One | Floating)

class val Mask
  let zeroes: USize
  let ones: USize

  new val create(zeroes': USize, ones': USize) =>
    zeroes = zeroes'
    ones = ones'

class val MaskSuperposition
  let masks: Array[Mask] val

  new val create(masks': Array[Mask] val) =>
    masks = masks'

class val Write
  let address: USize
  let value: USize

  new val create(address': USize, value': USize) =>
    address = address'
    value = value'

type Program is Array[(MaskSuperposition | Write)] val

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    try
      let collector = LineCollector[(MaskSuperposition | Write)](orchestrator, Parser.create()?)
      let solution = Solution(orchestrator)
      orchestrator.start[Program](collector, solution)
    else
      orchestrator.fail("Could not construct the parser.")
    end

class Parser is SingleItemParser[(MaskSuperposition | Write)]
  let _mask_parser: Regex val = recover val Regex("^mask = ([01X]+)$")? end
  let _mem_parser: Regex val = recover val Regex("^mem\\[(\\d+)\\] = (\\d+)$")? end

  new iso create() ? =>
    None

  fun parse(line: String val): (MaskSuperposition | Write) ? =>
    try
      let matched = recover val _mask_parser(line)? end
      let mask_chars = recover val matched(1)?.array() end
      let mask = recover val
        Iter[U8](mask_chars.values()).map[MaskBit]({ (ch) ? =>
          match ch
          | '0' => Zero
          | '1' => One
          | 'X' => Floating
          else error
          end
        }).collect[Array[MaskBit]](Array[MaskBit])
      end
      let floating_indices = recover val
        let floating_indices = Iter[(USize, MaskBit)](mask.pairs())
          .filter_map[USize]({ (pair) =>
            (let index, let bit) = pair
            if bit is Floating then
              index
            else
              None
            end
          })
          .collect[Array[USize]](Array[USize])
        floating_indices.reverse_in_place()
        floating_indices
      end
      let floating_count = 1 << floating_indices.size()
      let masks = recover val
        Iter[USize](collections.Range(0, floating_count)).map[Mask]({ (i) ? =>
          let floating_mask = collections.Map[USize, U8]
          for (j, floating_index) in floating_indices.pairs() do
            floating_mask(floating_index) = ((i >> j) and 1).u8()
          end
          let zeroes = Iter[(USize, MaskBit)](mask.pairs()).fold_partial[USize](0, { (acc, pair) ? =>
            (let j, let ch) = pair
            match ch
            | Zero => (acc << 1) or 1
            | One => (acc << 1) or 1
            | Floating => (acc << 1) or (floating_mask(j)?.usize())
            end
          })?
          let ones = Iter[(USize, MaskBit)](mask.pairs()).fold_partial[USize](0, { (acc, pair) ? =>
            (let j, let ch) = pair
            match ch
            | Zero => (acc << 1)
            | One => (acc << 1) or 1
            | Floating => (acc << 1) or (floating_mask(j)?.usize())
          end
          })?
          Mask(zeroes, ones)
        }).collect[Array[Mask]](Array[Mask])
      end
      MaskSuperposition(masks)
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
    var current_masks: Array[Mask] val = recover Array[Mask] end
    let memory = collections.Map[USize, USize]
    for instruction in program.values() do
      match instruction
      | let mask_superposition: MaskSuperposition =>
        current_masks = mask_superposition.masks
      | let write: Write =>
        for mask in current_masks.values() do
          let address = (write.address and mask.zeroes) or mask.ones
          memory(address) = write.value
        end
      end
    end
    let result = Iter[USize](memory.values()).fold[USize](0, { (sum, value) => sum + value })
    _answer.answer(result)
