use "collections"
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
    orchestrator.start(
      LineCollector[BoardingPass](
        Solution(orchestrator),
        orchestrator,
        Parser
      )
    )

class Parser is LineParser[BoardingPass]
  fun parse(line: String): BoardingPass ? =>
    let row = find(0, 128, 'F', 'B', line.substring(0, 7))?
    let column = find(0, 8, 'L', 'R', line.substring(7, 10))?
    BoardingPass(row, column)

  fun find(lowest: USize, highest: USize, low_char: U8, high_char: U8, input: String): USize ? =>
    let start = (lowest, highest, (highest - lowest) / 2)
    (let low, let high, _) = Iter[U8](input.values()).fold[(USize, USize, USize)](start, {(range, c) =>
      match range
      | (_, _, 0) => (0, 0, 0)
      | (let low: USize, let high: USize, let diff: USize) =>
        if c == low_char then
          (low, high - diff, diff / 2)
        elseif c == high_char then
          (low + diff, high, diff / 2)
        else
          (0, 0, 0)
        end
      end
    })
    if (low + 1) == high then low else error end

actor Solution is ASolution[Array[BoardingPass] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be solve(items: Array[BoardingPass] val) =>
    var seat_ids: Set[SeatId] ref = Set[SeatId]
    for id in Iter[BoardingPass](items.values()).map[SeatId]({ (pass) => pass.seat_id() }) do
      seat_ids = seat_ids.add(id)
    end
    let min_seat_id = Iter[SeatId](seat_ids.values())
      .fold[SeatId](USize.max_value(), { (acc, x) => acc.min(x) })
    let max_seat_id = Iter[SeatId](seat_ids.values())
      .fold[SeatId](USize.min_value(), { (acc, x) => acc.max(x) })
    let available_seat_ids = Iter[SeatId](Range(min_seat_id, max_seat_id + 1))
      .filter({ (id) => not seat_ids.contains(id) })
    for id in available_seat_ids do
      _answer.answer(id)
    end
