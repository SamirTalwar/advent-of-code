use collections = "collections"
use "collections/persistent"
use "itertools"
use "regex"

type Graph is Map[Luggage, Set[Luggage]]

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
  let contained: Array[Luggage] val

  new val create(container': Luggage, contained': Array[Luggage] val) =>
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
  let _contained_parser: Regex val = recover val Regex("^\\d+ ([a-z ]+) bags?$")? end

  new iso create() ? =>
    None

  fun parse(line: String): LuggageRule ? =>
    let root_matched = recover val _root_parser(line)? end
    let container_matched = _container_parser(root_matched(1)?)?
    let container = Luggage(container_matched(1)?)
    let contained_string = recover val root_matched(2)? end
    let contained: Array[Luggage] val = recover
      if _contained_none_parser == contained_string then
        Array[Luggage]
      else
        let contained_array = recover Array[Luggage] end
        for s in contained_string.split_by(", ").values() do
          let contained_matched = _contained_parser(s)?
          contained_array.push(Luggage(contained_matched(1)?))
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

    let contained = Luggage("shiny gold")
    try
      let result = _find_containers(contained, graph)? - contained
      _answer.answer(result.size())
    else
      _answer.fail("Could not find an answer.")
    end

  fun _compute_graph(rules: Array[LuggageRule] box): Graph =>
    var graph = Graph
    for rule in rules.values() do
      graph = graph.update(rule.container, graph.get_or_else(rule.container, Set[Luggage]))
      for contained in rule.contained.values() do
        let existing = graph.get_or_else(contained, Set[Luggage])
        graph = graph.update(contained, existing.add(rule.container))
      end
    end
    consume graph

  fun _find_containers(luggage: Luggage, graph: Graph): Set[Luggage] ? =>
    var containers = Set[Luggage].add(luggage)
    for container in graph(luggage)?.values() do
      containers = containers or _find_containers(container, graph)?
    end
    containers

  fun _log_rules(rules: Array[LuggageRule] val) =>
    _answer.answer("Rules:")
    for rule in rules.values() do
      _answer.answer(rule.container.color)
      for contained in rule.contained.values() do
        _answer.answer("  contains " + contained.color)
      end
      _answer.answer("")
    end

  fun _log_graph(graph: Graph) =>
    _answer.answer("Graph:")
    for (contained, containers) in graph.pairs() do
      _answer.answer(contained.color)
      for container in containers.values() do
        _answer.answer("  -> " + container.color)
      end
      _answer.answer("")
    end
