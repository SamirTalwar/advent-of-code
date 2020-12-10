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
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be solve(numbers: Array[USize] val) =>
    let numbers_ref = Array[USize](numbers.size() + 2)
    numbers_ref.push(0)
    numbers.copy_to(numbers_ref, 0, 1, numbers.size())
    let max = Fold[USize](numbers.values()).through1(0, { (a, b) => a.max(b) })
    numbers_ref.push(max + 3)
    let sorted = collections.Sort[Array[USize], USize](numbers_ref)
    let pairs = Iter[USize](sorted.values()).zip[USize](Iter[USize](sorted.values()).skip(1))
    var ones: USize = 0
    var threes: USize = 0
    for (a, b) in pairs do
      if (b - a) == 1 then
        ones = ones + 1
      elseif (b - a) == 3 then
        threes = threes + 1
      end
    end
    _answer.answer(ones * threes)
