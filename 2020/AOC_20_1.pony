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

type Grid is Array[Array[Cell] val] val
type Edges is Array[Array[Cell] val]

primitive Cells
  fun arrays_equal(a: box->Array[Cell], b: box->Array[Cell]): Bool =>
    (a.size() == b.size()) and
    Iter[Cell](a.values()).zip[Cell](b.values())
      .all({ (pair) =>
        match pair
        | (Off, Off) => true
        | (On, On) => true
        else false
        end
      })

class val Tile is Stringable
  let id: TileId
  let width: USize
  let height: USize
  let _grid: Grid
  let _transposed: Grid

  new val create(id': TileId, grid: Grid) ? =>
    id = id'
    height = grid.size()
    if height == 0 then
      error
    end
    width = grid(0)?.size()
    if not Iter[Array[Cell] val](grid.values()).all({ (row) => row.size() == width }) then
      error
    end
    _grid = grid
    _transposed = _compute_transposed(grid)?

  new val _create(id': TileId, width': USize, height': USize, grid: Grid, transposed: Grid) =>
    id = id'
    width = width'
    height = height'
    _grid = grid
    _transposed = transposed

  fun tag _compute_transposed(grid: Grid): Grid ? =>
    recover
      let w = grid(0)?.size()
      let transposed = Array[Array[Cell] val](w)
      for x in collections.Range(0, w) do
        let column = recover iso Array[Cell] end
        for row in grid.values() do
          column.push(row(x)?)
        end
        transposed.push(recover consume column end)
      end
      transposed
    end

  fun edges(): Edges ? =>
    let top = _grid(0)?
    let top_reversed = recover val top.reverse() end
    let bottom = _grid(_grid.size() - 1)?
    let bottom_reversed = recover val bottom.reverse() end
    let left = _transposed(0)?
    let left_reversed = recover val left.reverse() end
    let right = _transposed(_transposed.size() - 1)?
    let right_reversed = recover val right.reverse() end
    [top; top_reversed; right; right_reversed; bottom; bottom_reversed; left; left_reversed]

  fun string(): String iso^ =>
    "\n".join(Iter[String].chain([
      Iter[String].maybe("Tile " + id.string() + ":")
      Iter[Array[Cell] val](_grid.values()).map[String]({ (row) =>
        "".join(Iter[Cell](row.values()).map[String]({ (cell) =>
          cell.string()
        }))
      })
    ].values()))

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = MultipleLineCollector[Tile](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Array[Tile] val](collector, solution)

class Parser is MultipleItemParser[Tile]
  fun parse(lines: Array[String] val): Tile ? =>
    let id = lines(0)?.split()(1)?.split(":")(0)?.usize()?
    let grid = recover
      Iter[String](lines.values())
        .skip(1)
        .map[Array[Cell] val]({ (line) =>
          recover
            Iter[U8](line.array().values())
              .map[Cell]({ (char) ? =>
                match char
                | '.' => Off
                | '#' => On
                else error
                end
              })
              .collect(Array[Cell])
          end
        })
        .collect(Array[Array[Cell] val])
    end
    Tile(id, consume grid)?

actor Solution is Solve[Array[Tile] val]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(tiles: Array[Tile] val) =>
    let matches = recover val
      Iter[Tile](tiles.values())
        .map[(TileId, Set[TileId])]({ (tile) ? =>
          let edges = tile.edges()?
          var matching_tiles = Set[TileId]
          for matching_tile in tiles.values() do
            if not (matching_tile is tile) then
              for matching_edge in matching_tile.edges()?.values() do
                for edge in edges.values() do
                  if Cells.arrays_equal(edge, matching_edge) then
                    matching_tiles = matching_tiles + matching_tile.id
                  end
                end
              end
            end
          end
          (tile.id, matching_tiles)
        })
        .collect(Array[(TileId, Set[TileId])])
    end

    var result: USize = 1
    for (tile_id, matching_tiles) in matches.values() do
      if matching_tiles.size() == 2 then
        result = result * tile_id
      end
    end
    _answer.answer(result)
