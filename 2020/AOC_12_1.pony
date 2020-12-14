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

class val Ship
  let position: Position
  let direction: Direction

  new val create(position': Position, direction': Direction) =>
    position = position'
    direction = direction'

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
    solve_next(instructions, 0, Ship(Position(0, 0), East))

  be solve_next(instructions: Array[Instruction] val, index: USize, ship: Ship) =>
    if index == instructions.size() then
      _answer.answer(ship.position.x.abs() + ship.position.y.abs())
    else
      try
        let instruction = instructions(index)?
        let new_ship = match instruction.action
        | North =>
          Ship(ship.position.move(North, instruction.distance), ship.direction)
        | East =>
          Ship(ship.position.move(East, instruction.distance), ship.direction)
        | South =>
          Ship(ship.position.move(South, instruction.distance), ship.direction)
        | West =>
          Ship(ship.position.move(West, instruction.distance), ship.direction)
        | Left =>
          Ship(ship.position, rotate_left(ship.direction, instruction.distance / 90))
        | Right =>
          Ship(ship.position, rotate_right(ship.direction, instruction.distance / 90))
        | Forward =>
          Ship(ship.position.move(ship.direction, instruction.distance), ship.direction)
        end
        solve_next(instructions, index + 1, new_ship)
      else
        _answer.fail("Failed.")
      end
    end

  fun rotate_left(direction: Direction, amount: ISize): Direction =>
    if amount == 0 then
      direction
    else
      let next = match direction
      | North => West
      | West => South
      | South => East
      | East => North
      end
      rotate_left(next, amount - 1)
    end

  fun rotate_right(direction: Direction, amount: ISize): Direction =>
    if amount == 0 then
      direction
    else
      let next = match direction
      | North => East
      | East => South
      | South => West
      | West => North
      end
      rotate_right(next, amount - 1)
    end
