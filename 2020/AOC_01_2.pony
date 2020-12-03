use "buffered"

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

actor Collector
  let _solution: Solution tag
  let _escape: Escape tag
  var _items: Array[USize] iso = recover Array[USize] end
  var _failed: Bool = false

  new create(solution: Solution tag, escape: Escape tag) =>
    _solution = solution
    _escape = escape

  be gather(line: String) =>
    try
      let item = line.usize()?
      _items.push(item)
    else
      _escape.fail("Invalid item.")
      _failed = true
    end

  be ready() =>
    if not _failed then
      _solution.solve(_items = recover Array[USize] end)
    end

actor Solution
  let _answer: Answer[USize] tag

  new create(answer: Answer[USize] tag) =>
    _answer = answer

  be solve(items: Array[USize] val) =>
    for a in items.values() do
      for b in items.values() do
        for c in items.values() do
          if (a + b + c) == 2020 then
            _answer.answer(a * b * c)
            return
          end
        end
      end
    end
