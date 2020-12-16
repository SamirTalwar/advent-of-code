use collections = "collections"
use "itertools"

type Timestamp is USize

class val Entry is Comparable[Entry]
  let index: USize
  let timestamp: USize

  new val create(index': USize, timestamp': USize) =>
    index = index'
    timestamp = timestamp'

  fun eq(that: box->Entry): Bool =>
    (this.index == that.index) and (this.timestamp == that.timestamp)

  fun lt(that: box->Entry): Bool =>
    (this.timestamp < that.timestamp) or ((this.timestamp == that.timestamp) and (this.index < that.index))

type Problem is Array[Entry] val

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = OneShotCollector[Problem](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Problem](collector, solution)

class Parser is MultipleItemParser[Problem]
  fun parse(lines: Array[String] val): Problem ? =>
    recover
      Iter[String](lines(1)?.split_by(",").values())
        .zip[Timestamp](collections.Range(0, Timestamp.max_value()))
        .filter({ (input) =>
          try
            input._1.usize()?
            true
          else
            false
          end
        })
        .map[Entry]({ (pair) ? => Entry(pair._2, pair._1.usize()?) })
        .collect[Array[Entry]](Array[Entry])
    end

actor Solution is Solve[Problem]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(problem: Problem) =>
    let sorted = recover val
      let cloned = problem.clone()
      let sorted = collections.Sort[Array[Entry], Entry](cloned)
      sorted.reverse()
    end
    try
      let max_entry = sorted(0)?
      let t = max_entry.timestamp - max_entry.index
      solve_next(sorted, 1, t, max_entry.timestamp)
    else
      _answer.fail("Failed.")
    end

  be solve_next(problem: Problem, index: USize, t: Timestamp, inc: Timestamp) =>
    let entry = try
      problem(index)?
    else
      _answer.answer(t)
      return
    end

    if ((t + entry.index) % entry.timestamp) == 0 then
      let inc' = inc * entry.timestamp
      solve_next(problem, index + 1, t, inc')
    else
      let t' = t + inc
      solve_next(problem, index, t', inc)
    end
