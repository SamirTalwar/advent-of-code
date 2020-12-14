use "itertools"

primitive Open
primitive Tree
type Square is (Open | Tree)
type Row is Array[Square] val
type Grid is Array[Row] val

class Map
  let grid: Grid

  new create(grid': Grid) =>
    grid = consume grid'

  fun rows(): USize =>
    grid.size()

  fun apply(x: USize, y: USize): Square ? =>
    let row = grid(y)?
    row(x % row.size())?

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = GridCollector[Square](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Grid](collector, solution)

class Parser is CellParser[Square]
  fun parse(character: U8): Square ? =>
    match character
      | '.' => Open
      | '#' => Tree
      else error
    end

actor Solution is Solve[Grid]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(rows: Grid) =>
    let row_count = rows.size()
    let map = Map(rows)
    var y :USize = 0
    var x: USize = 0
    var trees: USize = 0
    try
      while y < row_count do
        if map(x, y)? is Tree then
          trees = trees + 1
        end
        x = x + 3
        y = y + 1
      end
      _answer.answer(trees)
    else
      _answer.fail("Lookup error.")
    end
