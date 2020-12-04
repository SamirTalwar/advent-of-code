use "itertools"
use "regex"

class val Policy
  let letter: String
  let min: USize
  let max: USize

  new val create(letter': String, min': USize, max': USize) =>
    letter = letter'
    min = min'
    max = max'

  fun validate(password: String): Bool =>
    let count = password.count(letter)
    (count >= min) and (count <= max)

class val Password
  let password: String
  let policy: Policy

  new val create(password': String, policy': Policy) =>
    password = password'
    policy = policy'

  fun is_valid(): Bool =>
    policy.validate(password)

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    orchestrator.start(
      LineCollector[Password](
        Solution(orchestrator),
        orchestrator,
        Parser
      )
    )

class Parser is LineParser[Password val]
  fun parse(line: String): Password ? =>
    let parser: Regex val = recover Regex("^(\\d+)-(\\d+) ([a-z]): ([a-z]+)$")? end
    let parsed = parser(line)?
    let policy = Policy(where
      min' = parsed(1)?.usize()?,
      max' = parsed(2)?.usize()?,
      letter' = parsed(3)?
    )
    Password(where
      policy' = policy,
      password' = parsed(4)?
    )

actor Solution is ASolution[Array[Password] val]
  let _answer: Answer tag

  new create(answer: Answer tag) =>
    _answer = answer

  be solve(items: Array[Password] val) =>
    let result = Iter[Password](items.values())
      .filter({ (password) => password.is_valid() })
      .count()
    _answer.answer(result)
