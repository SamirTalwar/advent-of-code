use "collections/persistent"
use "itertools"

type FieldName is String
type FieldValue is USize
type Ticket is Vec[FieldValue]
type Tickets is Vec[Ticket]

primitive TicketParser
  fun parse(line: String): Ticket =>
    ToVec[USize].from_iter(
      Iter[String](line.split_by(",").values())
        .map[USize]({ (field_string) ? => field_string.usize()? }))

class val FieldRange
  let start_inclusive: USize
  let end_inclusive: USize

  new val create(start_inclusive': USize, end_inclusive': USize) =>
    start_inclusive = start_inclusive'
    end_inclusive = end_inclusive'

  fun box contains(value: USize): Bool =>
    (start_inclusive <= value) and (value <= end_inclusive)

type FieldRules is Map[FieldName, Vec[FieldRange]]

class val Puzzle
  let field_rules: FieldRules
  let my_ticket: Ticket
  let nearby_tickets: Tickets

  new val create(
    field_rules': FieldRules,
    my_ticket': Ticket,
    nearby_tickets': Tickets
  ) =>
    field_rules = field_rules'
    my_ticket = my_ticket'
    nearby_tickets = nearby_tickets'

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = SplitCollector3[FieldRules, Ticket, Tickets, Puzzle](
      orchestrator,
      FieldRulesCollector(orchestrator),
      MyTicketCollector(orchestrator),
      NearbyTicketsCollector(orchestrator),
      { (field_rules, my_ticket, nearby_tickets) => Puzzle(field_rules, my_ticket, nearby_tickets) }
    )
    let solution = Solution(orchestrator)
    orchestrator.start[Puzzle](collector, solution)

actor FieldRulesCollector is Collector[FieldRules]
  let _escape: Escape tag
  var _field_rules: FieldRules = recover FieldRules end
  var _failed: Bool = false

  new create(escape: Escape tag) =>
    _escape = escape

  be gather(line: String) =>
    try
      let split_by_colon = line.split_by(": ")
      let field_name = split_by_colon(0)?
      let field_ranges = recover
        ToVec[FieldRange].from_iter(
          Iter[String](split_by_colon(1)?.split_by(" or ").values())
            .map[FieldRange]({ (string) ? =>
              let split = string.split_by("-")
              FieldRange(split(0)?.usize()?, split(1)?.usize()?)
            }))
      end
      _field_rules = (_field_rules(field_name) = field_ranges)
    else
      _failed = true
      _escape.fail("Failure while parsing field rules:\n" + line)
    end

  be ready(solve: Solve[FieldRules]) =>
    if not _failed then
      solve(_field_rules)
    end

actor MyTicketCollector is Collector[Ticket]
  let _escape: Escape tag
  var _expecting_header: Bool = true
  var _my_ticket: (Ticket | None) = None
  var _failed: Bool = false

  new create(escape: Escape tag) =>
    _escape = escape

  be gather(line: String) =>
    if _expecting_header then
      _check_for_header(line)
    else
      if _my_ticket is None then
        _my_ticket = TicketParser.parse(line)
      else
        _failed = true
        _escape.fail("Expected the next section, but got:\n" + line)
      end
    end

  be ready(solve: Solve[Ticket]) =>
    if not _failed then
      match _my_ticket
      | None =>
        _escape.fail("My ticket was not found.")
      | let my_ticket: Ticket =>
        solve(my_ticket)
      end
    end

  fun ref _check_for_header(line: String) =>
    if try line(line.size() - 1)? != ':' else true end then
      _failed = true
      _escape.fail("Expected a header, but got:\n" + line)
    end
    _expecting_header = false

actor NearbyTicketsCollector is Collector[Tickets]
  let _escape: Escape tag
  var _expecting_header: Bool = true
  var _nearby_tickets: Tickets = recover Tickets end
  var _failed: Bool = false

  new create(escape: Escape tag) =>
    _escape = escape

  be gather(line: String) =>
    if _expecting_header then
      _check_for_header(line)
    else
      _nearby_tickets = _nearby_tickets.push(TicketParser.parse(line))
    end

  be ready(solve: Solve[Tickets]) =>
    if not _failed then
      solve(_nearby_tickets)
    end

  fun ref _check_for_header(line: String) =>
    if try line(line.size() - 1)? != ':' else true end then
      _failed = true
      _escape.fail("Expected a header, but got:\n" + line)
    end
    _expecting_header = false

actor Solution is Solve[Puzzle]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(puzzle: Puzzle) =>
    let result = Iter[Ticket](puzzle.nearby_tickets.values())
      .flat_map[USize]({ (ticket) =>
        let puzzle' = puzzle
        Iter[USize](ticket.values()).filter({ (value) =>
          let value' = value
          Iter[Vec[FieldRange]](puzzle'.field_rules.values()).all({ (ranges) =>
            let value'' = value'
            not Iter[FieldRange](ranges.values()).any({ (range) =>
              range.contains(value'')
            })
          })
        })
      })
      .fold[USize](0, { (sum, value) => sum + value })
    _answer.answer(result)
