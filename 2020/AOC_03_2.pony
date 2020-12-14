use "itertools"

primitive Open
primitive Tree
type Square is (Open | Tree)
type Row is Array[Square] val
type Grid is Array[Row] val

class val Position
  let x: USize
  let y: USize

  new val create(x': USize, y': USize) =>
    x = x'
    y = y'

class val Slope
  let x: USize
  let y: USize

  new val create(x': USize, y': USize) =>
    x = x'
    y = y'

  fun increment(position: Position): Position =>
    Position(position.x + x, position.y + y)

class Map
  let grid: Grid

  new create(grid': Grid) =>
    grid = consume grid'

  fun rows(): USize =>
    grid.size()

  fun apply(position: Position): Square ? =>
    let row = grid(position.y)?
    row(position.x % row.size())?

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    orchestrator.start(
      GridCollector[Square](
        Solution(orchestrator),
        orchestrator,
        Parser
      )
    )

class Parser is CellParser[Square]
  fun parse(character: U8): Square ? =>
    match character
    | '.' => Open
    | '#' => Tree
    else error
  end

actor Solution is ASolution[Grid]
  let _slopes: Array[Slope] = [
    Slope(1, 1)
    Slope(3, 1)
    Slope(5, 1)
    Slope(7, 1)
    Slope(1, 2)
  ]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be solve(rows: Grid) =>
    let row_count = rows.size()
    let map = Map(rows)
    var trees: USize = 1
    try
      for slope in _slopes.values() do
        var slope_trees: USize = 0
        var position = Position(0, 0)
        while position.y < row_count do
          if map(position)? is Tree then
            slope_trees = slope_trees + 1
          end
          position = slope.increment(position)
        end
        trees = trees * slope_trees
      end
      _answer.answer(trees)
    else
      _answer.fail("Lookup error.")
    end
