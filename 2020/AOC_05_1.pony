use "itertools"

type Row is USize
type Column is USize
type SeatId is USize

class val BoardingPass
  let row: Row
  let column: Column

  new val create(row': Row, column': Column) =>
    row = row'
    column = column'

  fun val seat_id(): SeatId =>
    (row * 8) + column

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = LineCollector[BoardingPass](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[BoardingPass] val](collector, solution)

class Parser is LineParser[BoardingPass]
  fun parse(line: String): BoardingPass ? =>
    let row = find('F', 'B', line.substring(0, 7))?
    let column = find('L', 'R', line.substring(7, 10))?
    BoardingPass(row, column)

  fun find(low_char: U8, high_char: U8, input: String): USize ? =>
    Iter[U8](input.values()).fold_partial[USize](0, {(n, char) ? =>
      match char
      | let c: U8 if c == low_char => n << 1
      | let c: U8 if c == high_char => (n << 1) + 1
      else error
      end
    })?

actor Solution is Solve[Array[BoardingPass] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be apply(items: Array[BoardingPass] val) =>
    let max_seat_id = Iter[BoardingPass](items.values())
      .map[SeatId]({ (pass) => pass.seat_id() })
      .fold[SeatId](USize.min_value(), { (acc, x) => acc.max(x) })
    _answer.answer(max_seat_id)
