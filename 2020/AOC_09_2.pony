use collections = "collections"
use "itertools"

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let parser = Parser.create()
    let collector = LineCollector[USize](Solution(orchestrator), orchestrator, consume parser)
    orchestrator.start(collector)

class Parser is LineParser[USize]
  fun parse(line: String): USize ? =>
    line.usize()?

actor Solution is ASolution[Array[USize] val]
  let _window_size: USize = 25
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be solve(numbers: Array[USize] val) =>
    try
      let invalid_number = _find_invalid_number(numbers)?
      for i in collections.Range(0, numbers.size()) do
        var sum: USize = 0
        var j: USize = i
        while (j < numbers.size()) and (sum < invalid_number) do
          sum = sum + numbers(j)?
          j = j + 1
        end
        if ((j - i) >= 2) and (sum == invalid_number) then
          let smallest = Fold[USize](Iter[USize](numbers.values()).skip(i).take(j - i)).through1(0, { (a, b) => a.min(b) })
          let largest  = Fold[USize](Iter[USize](numbers.values()).skip(i).take(j - i)).through1(0, { (a, b) => a.max(b) })
          _answer.answer(smallest + largest)
        end
      end
    else
      _answer.fail("Failed.")
    end

  fun _find_invalid_number(numbers: Array[USize] val): USize ? =>
    let last = Array[USize].create(_window_size)
    numbers.copy_to(last, 0, 0, _window_size)
    for i in collections.Range(_window_size, numbers.size()) do
      let value = numbers(i)?

      if not _search(last, value) then
        return value
      end

      let cursor = i % _window_size
      last(cursor)? = value
    end
    error

  fun _search(last: Array[USize] ref, value: USize): Bool =>
    for a in last.values() do
      for b in last.values() do
        if (a + b) == value then
          return true
        end
      end
    end
    false
