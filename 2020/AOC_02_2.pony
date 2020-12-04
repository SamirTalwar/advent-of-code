use "itertools"
use "regex"

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
      position_a' = parsed(1)?.usize()? - 1,
      position_b' = parsed(2)?.usize()? - 1,
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
    let result = Iter[Password](items.values()).filter({ (password) => password.is_valid() }).count()
    _answer.answer(result)
