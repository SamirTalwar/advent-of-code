use collections = "collections"

primitive North
primitive East
primitive South
primitive West
primitive Left
primitive Right
primitive Forward
type Direction is (North | East | South | West)
type Action is (Direction | Left | Right | Forward)
type Distance is ISize

class val Instruction
  let action: Action
  let distance: Distance

  new val create(action': Action, distance': Distance) =>
    action = action'
    distance = distance'

class val Position
  let x: ISize
  let y: ISize

  new val create(x': ISize, y': ISize) =>
    x = x'
    y = y'

  fun move(direction: Direction, distance: Distance): Position =>
    match direction
    | North =>
      Position(x, y - distance)
    | East =>
      Position(x + distance, y)
    | South =>
      Position(x, y + distance)
    | West =>
      Position(x - distance, y)
    end

  fun string(): String =>
    "(" + x.string() + ", " + y.string() + ")"

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = LineCollector[Instruction](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[Instruction] val](collector, solution)

class Parser is LineParser[Instruction]
  fun parse(line: String): Instruction ? =>
    let action = match line(0)?
    | 'N' => North
    | 'E' => East
    | 'S' => South
    | 'W' => West
    | 'L' => Left
    | 'R' => Right
    | 'F' => Forward
    else error
    end
    let distance = line.substring(1).isize()?
    Instruction(action, distance)

actor Solution is Solve[Array[Instruction] val]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(instructions: Array[Instruction] val) =>
    solve_next(instructions, 0, Position(0, 0), Position(10, -1))

  be solve_next(instructions: Array[Instruction] val, index: USize, ship: Position, waypoint: Position) =>
    if index == instructions.size() then
      _answer.answer(ship.x.abs() + ship.y.abs())
    else
      try
        let instruction = instructions(index)?
        (let new_ship, let new_waypoint) = match instruction.action
        | North =>
          (ship, waypoint.move(North, instruction.distance))
        | East =>
          (ship, waypoint.move(East, instruction.distance))
        | South =>
          (ship, waypoint.move(South, instruction.distance))
        | West =>
          (ship, waypoint.move(West, instruction.distance))
        | Left =>
          (ship, rotate_left(waypoint, ship, instruction.distance / 90))
        | Right =>
          (ship, rotate_right(waypoint, ship, instruction.distance / 90))
        | Forward =>
          let waypoint_diff = Position(waypoint.x - ship.x, waypoint.y - ship.y)
          var waypoint' = waypoint
          var ship' = ship
          for i in collections.Range(0, instruction.distance.usize()) do
            ship' = Position(waypoint'.x, waypoint'.y)
            waypoint' = Position(ship'.x + waypoint_diff.x, ship'.y + waypoint_diff.y)
          end
          (ship', waypoint')
        end
        solve_next(instructions, index + 1, new_ship, new_waypoint)
      else
        _answer.fail("Failed.")
      end
    end

  fun rotate_left(waypoint: Position, ship: Position, amount: ISize): Position =>
    if amount == 0 then
      waypoint
    else
      let next = Position(ship.x + (waypoint.y - ship.y), ship.y - (waypoint.x - ship.x))
      rotate_left(next, ship, amount - 1)
    end

  fun rotate_right(waypoint: Position, ship: Position, amount: ISize): Position =>
    if amount == 0 then
      waypoint
    else
      let next = Position(ship.x - (waypoint.y - ship.y), ship.y + (waypoint.x - ship.x))
      rotate_right(next, ship, amount - 1)
    end
