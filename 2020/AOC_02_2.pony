use "buffered"
use "itertools"
use "regex"

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

class val Policy
  let letter: String
  let position_a: USize
  let position_b: USize

  new val create(letter': String, position_a': USize, position_b': USize) =>
    letter = letter'
    position_a = position_a'
    position_b = position_b'

  fun validate(password: String): Bool =>
    let at_position_a = password.at(letter, position_a.isize())
    let at_position_b = password.at(letter, position_b.isize())
    (at_position_a and not at_position_b) or (not at_position_a and at_position_b)

class val Password
  let password: String
  let policy: Policy

  new val create(password': String, policy': Policy) =>
    password = password'
    policy = policy'

  fun is_valid(): Bool =>
    policy.validate(password)

actor Collector
  let _solution: Solution tag
  let _escape: Escape tag
  var _items: Array[Password] iso = recover Array[Password] end
  var _failed: Bool = false

  new create(solution: Solution tag, escape: Escape tag) =>
    _solution = solution
    _escape = escape

  be gather(line: String) =>
    try
      let parser: Regex val = recover Regex("^(\\d+)-(\\d+) ([a-z]): ([a-z]+)$")? end
      let parsed = parser(line)?
      let policy = Policy(where
        position_a' = parsed(1)?.usize()? - 1,
        position_b' = parsed(2)?.usize()? - 1,
        letter' = parsed(3)?
      )
      let item = Password(where
        policy' = policy,
        password' = parsed(4)?
      )
      _items.push(item)
    else
      _escape.fail("Invalid item: " + line)
      _failed = true
    end

  be ready() =>
    if not _failed then
      _solution.solve(_items = recover Array[Password] end)
    end

actor Solution
  let _answer: Answer[USize] tag

  new create(answer: Answer[USize] tag) =>
    _answer = answer

  be solve(items: Array[Password] val) =>
    let result = Iter[Password](items.values()).filter({ (password) => password.is_valid() }).count()
    _answer.answer(result)
