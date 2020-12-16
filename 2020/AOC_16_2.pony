use collections = "collections"
use "collections/persistent"
use "itertools"

type FieldName is String
type FieldValue is USize
type Ticket is Vec[FieldValue]
type Tickets is Vec[Ticket]

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
    let collector = PuzzleCollector(orchestrator)
    let solution = Solution(orchestrator)
    orchestrator.start[Puzzle](collector, solution)

primitive FieldRulesMode
primitive MyTicketMode
primitive NearbyTicketsMode
primitive DoneMode
type CollectorMode is (FieldRulesMode | MyTicketMode | NearbyTicketsMode | DoneMode)

actor PuzzleCollector is Collector[Puzzle]
  let _escape: Escape tag
  var _mode: CollectorMode = FieldRulesMode
  var _expecting_header: Bool = false
  var _field_rules: FieldRules = recover FieldRules end
  var _my_ticket: (Ticket | None) = None
  var _nearby_tickets: Tickets = recover Tickets end
  var _failed: Bool = false

  new create(escape: Escape tag) =>
    _escape = escape

  be gather(line: String) =>
    if not _failed then
      match _mode
      | FieldRulesMode =>
        if line == "" then
          _mode = MyTicketMode
          _expecting_header = true
        else
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
        end
      | MyTicketMode =>
        if line == "" then
          _mode = NearbyTicketsMode
          _expecting_header = true
        elseif _expecting_header then
          _check_for_header(line)
        else
          if _my_ticket is None then
            _my_ticket = _parse_ticket(line)
          else
            _failed = true
            _escape.fail("Expected the next section, but got:\n" + line)
          end
        end
      | NearbyTicketsMode =>
        if line == "" then
          _mode = DoneMode
        elseif _expecting_header then
          _check_for_header(line)
        else
          _nearby_tickets = _nearby_tickets.push(_parse_ticket(line))
        end
      | DoneMode =>
        _failed = true
        _escape.fail("Thought I was done, but encountered:\n" + line)
      end
    end

  be ready(solve: Solve[Puzzle]) =>
    if not _failed then
      match _my_ticket
      | None =>
        _escape.fail("My ticket was not found.")
      | let my_ticket: Ticket =>
        solve(Puzzle(
          _field_rules,
          my_ticket,
          _nearby_tickets
        ))
      end
    end

  fun ref _check_for_header(line: String) =>
    if try line(line.size() - 1)? != ':' else true end then
      _failed = true
      _escape.fail("Expected a header, but got:\n" + line)
    end
    _expecting_header = false

  fun ref _parse_ticket(line: String): Ticket =>
    ToVec[USize].from_iter(
      Iter[String](line.split_by(",").values())
        .map[USize]({ (field_string) ? => field_string.usize()? }))

actor Solution is Solve[Puzzle]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(puzzle: Puzzle) =>
    try
      let valid_tickets = recover val
        Iter[Ticket](puzzle.nearby_tickets.values())
          .filter({ (ticket) =>
            let puzzle' = puzzle
            Iter[USize](ticket.values()).all({ (value) =>
              let value' = value
              Iter[Vec[FieldRange]](puzzle'.field_rules.values()).any({ (ranges) =>
                let value'' = value'
                Iter[FieldRange](ranges.values()).any({ (range) =>
                  range.contains(value'')
                })
              })
            })
          })
          .collect[Array[Ticket]](Array[Ticket])
      end

      let column_count = valid_tickets(0)?.size()

      let possibilities = recover val
        let possibilities = Iter[(FieldName, Vec[FieldRange])](puzzle.field_rules.pairs())
          .map[(FieldName, Set[USize])]({ (pair) =>
            (let name, let ranges) = pair
            let valid_tickets' = valid_tickets
            let columns = ToSet[USize].from_iter(
              Iter[USize](collections.Range(0, column_count)).filter({ (column) =>
                let ranges' = ranges
                Iter[Ticket](valid_tickets'.values()).all({ (ticket) =>
                  let column' = column
                  Iter[FieldRange](ranges'.values()).any({ (range) ? =>
                    range.contains(ticket(column')?)
                  })
                })
              }))
            (name, columns)
          })
          .collect[Array[(FieldName, Set[USize])]](Array[(FieldName, Set[USize])])

        while Iter[(FieldName, Set[USize])](possibilities.values()).any({ (pair) => pair._2.size() > 1 }) do
          let solos = ToSet[USize].from_iter(
            Iter[(FieldName, Set[USize])](possibilities.values())
              .map[Set[USize]]({ (pair) => pair._2 })
              .filter_map[USize]({ (columns) ? =>
                if columns.size() == 1 then columns.values().next()? else None end
              }))
          for (i, (name, columns)) in possibilities.pairs() do
            if columns.size() > 1 then
              possibilities(i)? = (name, columns.without(solos))
            end
          end
        end

        possibilities
      end

      let fields = recover
        var fields = Map[USize, FieldName]
        for (name, columns) in possibilities.values() do
          fields = (fields(columns.values().next()?) = name)
        end
        fields
      end

      let result = Iter[(USize, USize)](puzzle.my_ticket.pairs())
        .map[(FieldName, USize)]({ (pair) ? =>
          (let column, let value) = pair
          (fields(column)?, value)
        })
        .filter_map[USize]({ (pair) =>
          (let name, let value) = pair
          if name.substring(0, 10) == "departure " then
            value
          else
            None
          end
        })
        .fold[USize](1, { (sum, value) => sum * value })
      _answer.answer(result)
    else
      _answer.fail("Failed.")
    end
