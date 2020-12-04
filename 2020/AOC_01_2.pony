actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    orchestrator.start(
      LineCollector[USize](
        Solution(orchestrator),
        orchestrator,
        Parser
      )
    )

class Parser is LineParser[USize]
  fun parse(line: String): USize ? =>
    line.usize()?

actor Solution is ASolution[Array[USize] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
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
