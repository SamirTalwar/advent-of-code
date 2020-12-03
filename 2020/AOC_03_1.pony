use "buffered"
use "itertools"

interface Answer[T: Any val]
  be answer(value: T)

interface Escape
  be fail(message: String)

actor Main is Answer[USize]
  let _env: Env

  new create(env: Env) =>
    _env = env
    start()

  be start() =>
    let collector = Collector(Solution(this), this)
    _env.input(recover Notify(collector) end, 1024)

  be answer(value: USize) =>
    _env.out.print(value.string())

  be fail(message: String) =>
    _env.err.print(message)
    _env.exitcode(1)

class Notify is InputNotify
  let _collector: Collector
  let _reader: Reader = recover ref Reader end

  new create(collector: Collector) =>
    _collector = collector

  fun ref apply(data: Array[U8] iso) =>
    _reader.append(consume data)
    try
      while true do
        _collector.gather(_reader.line()?)
      end
    end

  fun ref dispose() =>
    _collector.ready()

primitive Open
primitive Tree
type Square is (Open | Tree)

class Map
  let grid: Array[Array[Square] val] val

  new create(grid': Array[Array[Square] val] val) =>
    grid = consume grid'

  fun rows(): USize =>
    grid.size()

  fun apply(x: USize, y: USize): Square ? =>
    let row = grid(y)?
    row(x % row.size())?

actor Collector
  let _open_char: U8 = '.'
  let _tree_char: I8 = '#'

  let _solution: Solution tag
  let _escape: Escape tag
  var _items: Array[Array[Square] val] iso = recover Array[Array[Square] val] end
  var _failed: Bool = false

  new create(solution: Solution tag, escape: Escape tag) =>
    _solution = solution
    _escape = escape

  be gather(line: String) =>
    let item: Array[Square] val = recover
      Iter[U8](line.values())
        .map[Square]({ (char): Square ? =>
          match char
            | '.' => Open
            | '#' => Tree
            else error
          end
        })
        .collect(Array[Square])
    end
    _items.push(consume item)

  be ready() =>
    if not _failed then
      let items: Array[Array[Square] val] iso = _items = recover Array[Array[Square] val] end
      _solution.solve(recover Map(consume items) end)
    end

actor Solution
  let _answer: (Answer[USize] tag & Escape tag)

  new create(answer: (Answer[USize] tag & Escape tag)) =>
    _answer = answer

  be solve(map: Map iso) =>
    let rows = map.rows()
    var y :USize = 0
    var x: USize = 0
    var trees: USize = 0
    try
      while y < rows do
        if map(x, y)? is Tree then
          trees = trees + 1
        end
        x = x + 3
        y = y + 1
      end
      _answer.answer(trees)
    else
      _answer.fail("Lookup error.")
    end
