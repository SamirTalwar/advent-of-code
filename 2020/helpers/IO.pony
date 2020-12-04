use "buffered"
use "itertools"

interface Answer
  be answer(value: Stringable val)

interface Escape
  be fail(message: String)

interface Solver
  be gather(line: String)

  be ready()

trait LineParser[Input]
  fun parse(line: String): Input ?

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

actor LineCollector[T: Any val]
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
