use "collections/persistent"
use "itertools"

type Index is USize
type Rule is (U8 | Vec[Vec[USize]])
type Rules is Map[Index, Rule]

primitive DebugRule
  fun string(index: Index, rule: Rule): String iso^ =>
    match rule
    | let character: U8 =>
      "".join([index.string(); ": \""; String.from_array([character]); "\""].values())
    | let rule': Vec[Vec[USize]] =>
      "".join([index.string(); ": "; " | ".join(Iter[Vec[USize]](rule'.values()).map[String]({ (x) => " ".join(x.values()) }))].values())
      end

type Message is String
type Messages is Set[Message]

class val Puzzle
  let rules: Rules
  let messages: Messages

  new val create(rules': Rules, messages': Messages) =>
    rules = rules'
    messages = messages'

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = SplitCollector2[Rules, Messages, Puzzle](
      orchestrator,
      RulesCollector(orchestrator),
      MessagesCollector(orchestrator),
      { (rules, messages) => Puzzle(rules, messages) }
    )
    let solution = Solution(orchestrator)
    orchestrator.start[Puzzle](collector, solution)

actor RulesCollector is Collector[Rules]
  let _escape: Escape tag
  var _rules: Rules = recover Rules end
  var _failed: Bool = false

  new create(escape: Escape tag) =>
    _escape = escape

  be gather(line: String) =>
    try
      let split = line.split_by(": ")
      let rule_string = split(1)?
      let rule = match rule_string(0)?
      | '"' => // " // Syntax highlighter doesn't handle a solo ".
        rule_string(1)?
      else
        ToVec[Vec[USize]].from_iter(
          Iter[String](rule_string.split_by(" | ").values())
            .map[Vec[USize]]({ (string) =>
              ToVec[USize].from_iter(
                Iter[String](string.split_by(" ").values())
                  .map[USize]({ (s) ? => s.usize()? })) }))
      end
      _rules = (_rules(split(0)?.usize()?) = rule)
    else
      _failed = true
      _escape.fail("Failure while parsing rules:\n" + line)
    end

  be ready(solve: Solve[Rules]) =>
    if not _failed then
      solve(_rules)
    end

actor MessagesCollector is Collector[Messages]
  let _escape: Escape tag
  var _messages: Messages = recover Messages end

  new create(escape: Escape tag) =>
    _escape = escape

  be gather(line: String) =>
    _messages = (_messages + line)

  be ready(solve: Solve[Messages]) =>
    solve(_messages)

actor Solution is Solve[Puzzle]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(puzzle: Puzzle) =>
    try
      let possibilities = Possibilities(0, puzzle.rules)?
      let valid = puzzle.messages and possibilities
      _answer.answer(valid.size())
    else
      _answer.fail("Failed.")
    end

primitive Possibilities
  fun apply(index: Index, rules: Rules): Iter[String] ? =>
    match rules(index)?
    | let character: U8 =>
      Iter[String].maybe(String.from_array([character]))
    | let rule: Vec[Vec[USize]] =>
      Iter[Vec[USize]](rule.values())
        .flat_map[String]({ (indices) =>
          let rules' = rules
          Possibilities.combine(Iter[USize](indices.values()).map[Iter[String]]({ (i) ? => Possibilities(i, rules')? })) })
      end

  fun combine(sets: Iter[Iter[String]]): Iter[String] =>
    try
      let first = ToSet[String].from_iter(sets.next()?)
      let rest = combine(sets)
      rest.flat_map[String]({ (s) =>
        Iter[String](first.values()).map[String]({ (f) =>
          f + s }) })
    else
      Iter[String].maybe("")
    end
