use "itertools"

type Timestamp is USize

class val Problem
  let earliest_timestamp: Timestamp
  let bus_ids: Array[Timestamp] val

  new val create(earliest_timestamp': Timestamp, bus_ids': Array[Timestamp] val) =>
    earliest_timestamp = earliest_timestamp'
    bus_ids = bus_ids'

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = OneShotCollector[Problem](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Problem](collector, solution)

class Parser is MultipleItemParser[Problem]
  fun parse(lines: Array[String] val): Problem ? =>
    let earliest_timestamp = lines(0)?.usize()?
    let bus_ids = recover
      Iter[String](lines(1)?.split_by(",").values())
        .filter({ (input) => try input.usize()?; true else false end })
        .map[Timestamp]({ (input) ? => input.usize()? })
        .collect[Array[Timestamp]](Array[Timestamp])
    end
    Problem(earliest_timestamp, consume bus_ids)

actor Solution is Solve[Problem]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be apply(problem: Problem) =>
    (let bus_id, let diff) = Iter[Timestamp](problem.bus_ids.values())
      .map[(Timestamp, USize)]({ (id) => (id, id - (problem.earliest_timestamp % id)) })
      .fold[(Timestamp, USize)]((0, USize.max_value()), { (min, diff) =>
        if diff._2 < min._2 then
          diff
        else
          min
        end
      })
    _answer.answer(bus_id * diff)
