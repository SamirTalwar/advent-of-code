use collections = "collections"
use "collections/persistent"
use "itertools"

type TileId is USize

primitive Off is Stringable
  fun string(): String iso^ =>
    ".".clone()

primitive On is Stringable
  fun string(): String iso^ =>
    "#".clone()

type Cell is (Off | On)

type Grid[T: Any val] is Vec[Vec[T]]

type Edge is String
type Edges is Set[Edge]

primitive Grids
  fun is_empty[T: Any val](grid: Grid[(T | None)]): Bool =>
    Iter[Vec[(T | None)] val](grid.values()).all({ (row) => Iter[(T | None)](row.values()).all({ (value) => value is None }) })

  fun total[T: Any val](grid: Grid[(T | None)]): Grid[T] ? =>
    var new_grid = Grid[T]
    for row in grid.values() do
      var new_row = Vec[T]
      for cell in row.values() do
        match cell
        | let value: T => new_row = new_row.push(value)
        | None => error
        end
      end
      new_grid = new_grid.push(new_row)
    end
    new_grid

  fun flip_horizontal[T: Any val](grid: Grid[T]): Grid[T] =>
    ToVec[Vec[T]].from_iter(
      Iter[Vec[T]](grid.values())
        .map[Vec[T]]({ (row) => row.reverse() }))

  fun flip_vertical[T: Any val](grid: Grid[T]): Grid[T] =>
    grid.reverse()

  fun string[T: Stringable val](grid: Grid[T]): String iso^ =>
    "\n".join(Iter[Vec[T]](grid.values()).map[String]({ (row) =>
      "".join(Iter[T](row.values()).map[String]({ (value) =>
        value.string()
      }))
    }))

primitive ComputeEdges
  fun from(grid: Grid[Cell], transposed: Grid[Cell]): Edges ? =>
    let top = _row_to_edge(grid(0)?)
    let top_reversed = top.reverse()
    let bottom = _row_to_edge(grid(grid.size() - 1)?)
    let bottom_reversed = bottom.reverse()
    let left = _row_to_edge(transposed(0)?)
    let left_reversed = left.reverse()
    let right = _row_to_edge(transposed(transposed.size() - 1)?)
    let right_reversed = right.reverse()
    ToSet[Edge].from_iterator([
      consume top
      consume top_reversed
      consume right
      consume right_reversed
      consume bottom
      consume bottom_reversed
      consume left
      consume left_reversed
    ].values())

  fun _row_to_edge(row: Vec[Cell]): Edge =>
    "".join(Iter[Cell](row.values()).map[String iso]({ (cell) => cell.string() }))

class val Tile is Stringable
  let id: TileId
  let width: USize
  let height: USize
  let _grid: Grid[Cell]
  let _transposed: Grid[Cell]
  let edges: Edges

  new val create(id': TileId, grid: Grid[Cell]) ? =>
    id = id'
    height = grid.size()
    if height == 0 then
      error
    end
    width = grid(0)?.size()
    if not Iter[Vec[Cell]](grid.values()).all({ (row) => row.size() == width }) then
      error
    end
    _grid = grid
    _transposed = _compute_transposed(grid)?
    edges = ComputeEdges.from(_grid, _transposed)?

  new val _create(id': TileId, width': USize, height': USize, grid: Grid[Cell], transposed: Grid[Cell]) ? =>
    id = id'
    width = width'
    height = height'
    _grid = grid
    _transposed = transposed
    edges = ComputeEdges.from(_grid, _transposed)?

  fun tag _compute_transposed(grid: Grid[Cell]): Grid[Cell] ? =>
    let w = grid(0)?.size()
    var transposed = Vec[Vec[Cell]]
    for x in collections.Range(0, w) do
      var column = Vec[Cell]
      for row in grid.values() do
        column = column.push(row(x)?)
      end
      transposed = transposed.push(column)
    end
    transposed

  fun transpose(): Tile ? =>
    Tile._create(id, height, width, _transposed, _grid)?

  fun string(): String iso^ =>
    "\n".join([
      "Tile " + id.string() + ":"
      Grids.string[Cell](_grid)
    ].values())

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = MultipleLineCollector[Tile](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[Tile] val](collector, solution)

class Parser is MultipleItemParser[Tile]
  fun parse(lines: Array[String] val): Tile ? =>
    let id = lines(0)?.split()(1)?.split(":")(0)?.usize()?
    let grid = ToVec[Vec[Cell]].from_iter(
      Iter[String](lines.values())
        .skip(1)
        .map[Vec[Cell]]({ (line) =>
          ToVec[Cell].from_iter(
            Iter[U8](line.array().values())
              .map[Cell]({ (char) ? =>
                match char
                | '.' => Off
                | '#' => On
                else error
                end
              }))
        }))
    Tile(id, consume grid)?

actor Solution is Solve[Array[Tile] val]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(tiles: Array[Tile] val) =>
    var tile_map = Map[TileId, Tile]
    for tile in tiles.values() do
      tile_map = (tile_map(tile.id) = tile)
    end
    _match_tiles(tile_map)

  be _match_tiles(tiles: Map[TileId, Tile]) =>
    var matches = Map[TileId, Set[TileId]]
    for tile in tiles.values() do
      _answer.debug(tile.id)
      var matching_tiles = Set[TileId]
      for matching_tile in tiles.values() do
        if not (matching_tile is tile) then
          if (tile.edges and matching_tile.edges).size() > 0 then
            matching_tiles = matching_tiles + matching_tile.id
          end
        end
      end
      matches = (matches(tile.id) = matching_tiles)
    end
    _arrange_tiles(tiles, matches)

  be _arrange_tiles(
    tiles: Map[TileId, Tile],
    matches: Map[TileId, Set[TileId]]
  ) =>
    try
      let square = tiles.size().f64().sqrt().usize()
      var start = Grid[(Tile | None)]
      var row = Vec[(Tile | None)]
      for _ in collections.Range(0, square) do
        row = row.push(None)
      end
      for _ in collections.Range(0, square) do
        start = start.push(row)
      end
      let arranged = match _arrange_tiles_from(consume start, tiles, matches)?
      | let grid: Grid[(Tile | None)] =>
        Grids.total[Tile](grid)?
      | None =>
        _answer.fail("No valid grid found.")
        error
      end
      _answer.answer(arranged.size())
    else
      _answer.fail("Failed.")
    end

  fun _arrange_tiles_from(
    current: Grid[(Tile | None)],
    tiles: Map[TileId, Tile],
    matches: Map[TileId, Set[TileId]]
  ): (Grid[(Tile | None)] | None) ? =>
    if Grids.is_empty[Tile](current) then
      (let start, _) = Iter[(TileId, Set[TileId])](matches.pairs()).find({ (pair) => pair._2.size() == 2 })?
      let next = (current(0)?(0)? = tiles(start)?)
    end
    None
