use "buffered"
use collections = "collections"
use "itertools"

interface Answer
  be answer(value: Stringable val)

  be debug(value: Stringable val)

interface Escape
  be fail(message: String)

interface Solver
  be gather(line: String)

  be ready()

trait LineParser[Input]
  fun parse(line: String): Input ?

trait MultipleLineParser[Input]
  fun parse(lines: Array[String] val): Input ?

trait CellParser[Input]
  fun parse(character: U8): Input ?

trait ASolution[Input: Any val]
  be solve(input: Input)

actor Orchestrator is (Answer & Escape)
  let _env: Env

  new create(env: Env) =>
    _env = env

  be start(solver: Solver tag) =>
    _env.input(recover Notify(solver) end, 1024)

  be answer(output: Stringable val) =>
    _env.out.print(output.string())

  be debug(output: Stringable val) =>
    _env.err.print(output.string())

  be fail(message: String) =>
    _env.err.print(message)
    _env.exitcode(1)

class Notify is InputNotify
  let _solver: Solver tag
  let _reader: Reader = recover ref Reader end

  new create(solver: Solver tag) =>
    _solver = solver

  fun ref apply(data: Array[U8] iso) =>
    _reader.append(consume data)
    try
      while true do
        _solver.gather(_reader.line()?)
      end
    end

  fun ref dispose() =>
    _solver.ready()

actor OneShotCollector[T: Any val] is Solver
  let _solution: ASolution[T val] tag
  let _escape: Escape tag
  let _parser: MultipleLineParser[T]
  var _lines: Array[String val] iso

  new create(solution: ASolution[T val] tag, escape: Escape tag, parser: MultipleLineParser[T] iso) =>
    _solution = solution
    _escape = escape
    _parser = consume parser
    _lines = recover Array[String val] end

  be gather(line: String) =>
    _lines.push(consume line)

  be ready() =>
    let lines: Array[String val] val = _lines = recover Array[String val] end
    try
      _solution.solve(_parser.parse(lines)?)
    else
      _escape.fail("Invalid input.")
    end

actor LineCollector[T: Any val] is Solver
  let _solution: ASolution[Array[T] val] tag
  let _escape: Escape tag
  let _parser: LineParser[T]
  var _items: Array[T] iso
  var _failed: Bool = false

  new create(solution: ASolution[Array[T] val] tag, escape: Escape tag, parser: LineParser[T] iso) =>
    _solution = solution
    _escape = escape
    _parser = consume parser
    _items = recover Array[T] end

  be gather(line: String) =>
    try
      let item: T val = _parser.parse(line) ?
      _items.push(consume item)
    else
      _escape.fail("Invalid item: " + line)
      _failed = true
    end

  be ready() =>
    if not _failed then
      let items: Array[T] iso = _items = recover Array[T] end
      _solution.solve(recover consume items end)
    end

actor MultipleLineCollector[T: Any val] is Solver
  let _solution: ASolution[Array[T] val] tag
  let _escape: Escape tag
  let _parser: MultipleLineParser[T]
  var _items: Array[T] iso
  var _current: Array[String] iso
  var _failed: Bool = false

  new create(solution: ASolution[Array[T] val] tag, escape: Escape tag, parser: MultipleLineParser[T] iso) =>
    _solution = solution
    _escape = escape
    _parser = consume parser
    _items = recover Array[T] end
    _current = recover Array[String] end

  fun ref parse_current() =>
    let current = _current = recover Array[String] end
    let lines = recover val consume current end
    try
      let item: T val = _parser.parse(lines) ?
      _items.push(consume item)
    else
      _escape.fail("Invalid item:\n" + "\n".join(lines.values()))
      _failed = true
    end

  be gather(line: String) =>
    if line != "" then
      _current.push(consume line)
    else
      parse_current()
    end

  be ready() =>
    if _current.size() > 0 then
      parse_current()
    end
    if not _failed then
      let items: Array[T] val = _items = recover Array[T] end
      _solution.solve(items)
    end

actor GridCollector[T: Any val]
  let _solution: ASolution[Array[Array[T] val] val] tag
  let _escape: Escape tag
  let _parser: CellParser[T]
  var _grid: Array[Array[T] val] iso
  var _failed: Bool = false

  new create(solution: ASolution[Array[Array[T] val] val] tag, escape: Escape tag, parser: CellParser[T] iso) =>
    _solution = solution
    _escape = escape
    _parser = consume parser
    _grid = recover Array[Array[T] val] end

  be gather(line: String) =>
    let row = recover iso Array[T] end
    for character in line.values() do
      try
        let item: T val = _parser.parse(character) ?
        row.push(consume item)
      else
        _escape.fail("Invalid character: " + String.from_array([character]))
        _failed = true
      end
    end
    _grid.push(recover consume row end)

  be ready() =>
    if not _failed then
      let grid: Array[Array[T] val] iso = _grid = recover Array[Array[T] val] end
      _solution.solve(recover consume grid end)
    end
