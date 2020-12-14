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
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be apply(items: Array[USize] val) =>
    for a in items.values() do
      for b in items.values() do
        if (a + b) == 2020 then
          _answer.answer(a * b)
          return
        end
      end
    end
