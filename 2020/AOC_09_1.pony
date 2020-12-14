use collections = "collections"

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = LineCollector[USize](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[USize] val](collector, solution)

class Parser is LineParser[USize]
  fun parse(line: String): USize ? =>
    line.usize()?

actor Solution is Solve[Array[USize] val]
  let _window_size: USize = 25
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(numbers: Array[USize] val) =>
    let last = Array[USize].create(_window_size)
    numbers.copy_to(last, 0, 0, _window_size)
    for i in collections.Range(_window_size, numbers.size()) do
      let value = try
        numbers(i)?
      else
        _answer.fail("Failed at index " + i.string() + ".")
        break
      end

      if not _search(last, value) then
        _answer.answer(value)
      end

      let cursor = i % _window_size
      try
        last(cursor)? = value
      else
        _answer.fail("Failed at index " + i.string() + ".")
        break
      end
    end

  fun _search(last: Array[USize] ref, value: USize): Bool =>
    for a in last.values() do
      for b in last.values() do
        if (a + b) == value then
          return true
        end
      end
    end
    false
