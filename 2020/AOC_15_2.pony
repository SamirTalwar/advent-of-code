use collections = "collections"
use "itertools"

type Turn is USize
type GameNumber is USize

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = CommaSeparatedCollector[GameNumber](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[GameNumber] val](collector, solution)

class Parser is SingleItemParser[GameNumber]
  fun parse(item: String): GameNumber ? =>
    item.usize()?

actor Solution is Solve[Array[GameNumber] val]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(starting_numbers: Array[GameNumber] val) =>
    try
      let result = Iter[GameNumber](Game(starting_numbers)).nth(30000000)?
      _answer.answer(result)
    else
      _answer.fail("Failed.")
    end

class ref Game is Iterator[GameNumber]
  let _starting_numbers: Array[GameNumber] val
  var _turn: Turn = 0
  var _most_recent: GameNumber = 0
  var _previous: Array[(None | Turn | (Turn, Turn))]

  new create(starting_numbers: Array[GameNumber] val) =>
    _starting_numbers = starting_numbers
    let max = Fold[GameNumber](starting_numbers.values()).through1(0, { (max, value) => max.max(value) })
    _previous = Array[(None | Turn | (Turn, Turn))].init(None where len = max + 1)

  fun ref has_next(): Bool =>
    true

  fun ref next(): GameNumber ? =>
    let value = if _turn < _starting_numbers.size() then
      let value = _starting_numbers(_turn)?
      _previous(value)? = _turn
      value
    else
      let value = match _previous(_most_recent)?
      | None =>
        error
      | let _: USize =>
        0
      | (let a: USize, let b: USize) =>
        b - a
      end

      if _previous.size() <= value then
        _previous.reserve(value + 1)
        while _previous.size() <= value do
          _previous.push(None)
        end
      end
      _previous(value)? = match _previous(value)?
      | None => _turn
      | let a: USize => (a, _turn)
      | (let a: USize, let b: USize) => (b, _turn)
      end
      value
    end
    _turn = _turn + 1
    _most_recent = value
    value
