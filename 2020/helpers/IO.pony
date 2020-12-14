use "buffered"
use collections = "collections"
use "itertools"

interface Answer
  be answer(value: Stringable val)

  be debug(value: Stringable val)

interface Escape
  be fail(message: String)

interface tag Collector[Input: Any val]
  be gather(line: String)

  be ready(solve: Solve[Input])

trait LineParser[Input]
  fun parse(line: String): Input ?

trait MultipleLineParser[Input]
  fun parse(lines: Array[String] val): Input ?

trait CellParser[Input]
  fun parse(character: U8): Input ?

interface tag Solve[Input: Any val]
  be apply(input: Input)

actor Orchestrator is (Answer & Escape)
  let _env: Env

  new create(env: Env) =>
    _env = env

  be start[Input: Any val](collector: Collector[Input], solve: Solve[Input]) =>
    _env.input(recover Notify[Input](collector, solve) end, 1024)

  be answer(output: Stringable val) =>
    _env.out.print(output.string())

  be debug(output: Stringable val) =>
    _env.err.print(output.string())

  be fail(message: String) =>
    _env.err.print(message)
    _env.exitcode(1)

class Notify[Input: Any val] is InputNotify
  let _collector: Collector[Input]
  let _solve: Solve[Input]
  let _reader: Reader = recover ref Reader end

  new create(collector: Collector[Input], solve: Solve[Input]) =>
    _collector = collector
    _solve = solve

  fun ref apply(data: Array[U8] iso) =>
    _reader.append(consume data)
    try
      while true do
        _collector.gather(_reader.line()?)
      end
    end

  fun ref dispose() =>
    _collector.ready(_solve)

actor OneShotCollector[T: Any val] is Collector[T]
  let _escape: Escape tag
  let _parser: MultipleLineParser[T]
  var _lines: Array[String val] iso

  new create(escape: Escape tag, parser: MultipleLineParser[T] iso) =>
    _escape = escape
    _parser = consume parser
    _lines = recover Array[String val] end

  be gather(line: String) =>
    _lines.push(consume line)

  be ready(solve: Solve[T]) =>
    let lines: Array[String val] val = _lines = recover Array[String val] end
    try
      solve(_parser.parse(lines)?)
    else
      _escape.fail("Invalid input.")
    end

actor LineCollector[T: Any val] is Collector[Array[T] val]
  let _escape: Escape tag
  let _parser: LineParser[T]
  var _items: Array[T] iso
  var _failed: Bool = false

  new create(escape: Escape tag, parser: LineParser[T] iso) =>
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

  be ready(solve: Solve[Array[T] val]) =>
    if not _failed then
      let items: Array[T] val = _items = recover Array[T] end
      solve(items)
    end

actor MultipleLineCollector[T: Any val] is Collector[Array[T] val]
  let _escape: Escape tag
  let _parser: MultipleLineParser[T]
  var _items: Array[T] iso
  var _current: Array[String] iso
  var _failed: Bool = false

  new create(escape: Escape tag, parser: MultipleLineParser[T] iso) =>
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

  be ready(solve: Solve[Array[T] val]) =>
    if _current.size() > 0 then
      parse_current()
    end
    if not _failed then
      let items: Array[T] val = _items = recover Array[T] end
      solve(items)
    end

actor GridCollector[T: Any val] is Collector[Array[Array[T] val] val]
  let _escape: Escape tag
  let _parser: CellParser[T]
  var _grid: Array[Array[T] val] iso
  var _failed: Bool = false

  new create(escape: Escape tag, parser: CellParser[T] iso) =>
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

  be ready(solve: Solve[Array[Array[T] val] val]) =>
    if not _failed then
      let grid: Array[Array[T] val] iso = _grid = recover Array[Array[T] val] end
      solve(recover consume grid end)
    end
