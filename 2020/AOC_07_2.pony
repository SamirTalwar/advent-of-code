use collections = "collections"
use "collections/persistent"
use "itertools"
use "regex"

type Count is USize
type Contained is Array[(Count, Luggage)]
type Graph is Map[Luggage, Contained val]

class val Luggage is (Equatable[Luggage] & collections.Hashable)
  let color: String val

  new val create(color': String val) =>
    color = color'

  fun eq(that: box->Luggage): Bool =>
    this.color == that.color

  fun hash(): USize =>
    color.hash()

class val LuggageRule
  let container: Luggage
  let contained: Contained val

  new val create(container': Luggage, contained': Contained val) =>
    container = container'
    contained = contained'

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    try
      let parser = Parser.create()?
      let collector = LineCollector[LuggageRule](orchestrator, consume parser)
      let solution = Solution(orchestrator)
      orchestrator.start[Array[LuggageRule] val](collector, solution)
    else
      orchestrator.fail("Could not construct the parser.")
    end

class Parser is SingleItemParser[LuggageRule]
  let _root_parser: Regex val = recover val Regex("^(.+) contain (.+)\\.")? end
  let _container_parser: Regex val = recover val Regex("^([a-z ]+) bags$")? end
  let _contained_none_parser: Regex val = recover val Regex("^no other bags$")? end
  let _contained_parser: Regex val = recover val Regex("^(\\d+) ([a-z ]+) bags?$")? end

  new iso create() ? =>
    None

  fun parse(line: String): LuggageRule ? =>
    let root_matched = recover val _root_parser(line)? end
    let container_matched = _container_parser(root_matched(1)?)?
    let container = Luggage(container_matched(1)?)
    let contained_string = recover val root_matched(2)? end
    let contained: Contained val = recover
      if _contained_none_parser == contained_string then
        Contained
      else
        let contained_array = recover Contained end
        for s in contained_string.split_by(", ").values() do
          let contained_matched = _contained_parser(s)?
          contained_array.push((contained_matched(1)?.usize()?, Luggage(contained_matched(2)?)))
        end
        consume contained_array
      end
    end
    LuggageRule(consume container, consume contained)

actor Solution is Solve[Array[LuggageRule] val]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(rules: Array[LuggageRule] val) =>
    // _log_rules(rules)

    let graph = _compute_graph(rules)
    // _log_graph(graph)

    let container = Luggage("shiny gold")
    try
      let result = _count_contained(container, graph)?
      _answer.answer(result)
    else
      _answer.fail("Could not find an answer.")
    end

  fun _compute_graph(rules: Array[LuggageRule] box): Graph =>
    var graph = Graph
    for rule in rules.values() do
      graph = graph.update(rule.container, rule.contained)
    end
    consume graph

  fun _count_contained(luggage: Luggage, graph: Graph): USize ? =>
    var counter: USize = 0
    for (count, contained) in graph(luggage)?.values() do
      counter = counter + count + (count * _count_contained(contained, graph)?)
    end
    counter

  fun _log_rules(rules: Array[LuggageRule] val) =>
    _answer.answer("Rules:")
    for rule in rules.values() do
      _answer.answer(rule.container.color)
      for (n, contained) in rule.contained.values() do
        _answer.answer("  contains " + n.string() + " " + contained.color)
      end
      _answer.answer("")
    end

  fun _log_graph(graph: Graph) =>
    _answer.answer("Graph:")
    for (container, contained) in graph.pairs() do
      _answer.answer(container.color)
      for (count, luggage) in contained.values() do
        _answer.answer("  -> " + count.string() + " " + luggage.color)
      end
      _answer.answer("")
    end
