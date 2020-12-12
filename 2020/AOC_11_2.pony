use collections = "collections"
use "itertools"

primitive Floor
primitive Empty
primitive Occupied
type Cell is (Floor | Empty | Occupied)

type Row is Array[Cell] val
type Grid is Array[Row] val

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let parser = Parser.create()
    let collector = GridCollector[Cell](Solution(orchestrator), orchestrator, consume parser)
    orchestrator.start(collector)

class Parser is CellParser[Cell]
  fun parse(character: U8): Cell ? =>
    match character
    | '.' => Floor
    | 'L' => Empty
    | '#' => Occupied
    else error
    end

actor Solution is ASolution[Array[Array[Cell] val] val]
  let _answer: (Answer tag & Escape tag)
  let _directions: Array[(USize, USize)] val = [(-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1)]

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be solve(grid: Grid) =>
    try
      let new_grid = step(grid)?
      if grids_equal(grid, new_grid) then
        _answer.answer(count_occupied(grid)?)
      else
        solve(new_grid)
      end
    else
      _answer.fail("Failed.")
    end

  fun step(grid: Grid): Grid ? =>
    let new_grid = recover iso Array[Array[Cell] val] end
    for y in collections.Range(0, grid.size()) do
      let row = grid(y)?
      let new_row = recover iso Array[Cell] end
      for x in collections.Range(0, row.size()) do
        let cell = row(x)?
        match cell
        | Floor =>
          new_row.push(cell)
        else
          let count = count_visible_occupied(x, y, grid)
          let new_cell = match count
          | 0 => Occupied
          | let c: USize if c >= 5 => Empty
          else cell
          end
          new_row.push(consume new_cell)
        end
      end
      new_grid.push(consume new_row)
    end
    recover consume new_grid end

  fun count_visible_occupied(x: USize, y: USize, grid: Grid): USize =>
    var count: USize = 0
    for direction in _directions.values() do
      var position = (x, y)
      while true do
        // Will underflow to `USize.max_value()`.
        position = (position._1 + direction._1, position._2 + direction._2)
        try
          match grid(position._2)?(position._1)?
          | Empty =>
            break
          | Occupied =>
            count = count + 1
            break
          end
        else
          break
        end
      end
    end
    count

  fun count_occupied(grid: Grid): USize ? =>
    var occupied_count: USize = 0
    for y in collections.Range(0, grid.size()) do
      for x in collections.Range(0, grid(y)?.size()) do
        match grid(y)?(x)?
        | Occupied =>
          occupied_count = occupied_count + 1
        end
      end
    end
    occupied_count

  fun grids_equal(grid_a: Grid, grid_b: Grid): Bool =>
    Iter[Row](grid_a.values())
      .zip[Row](grid_b.values())
      .all({ (row_pair) =>
        (let row_a, let row_b) = row_pair
        Iter[Cell](row_a.values())
          .zip[Cell](row_b.values())
          .all({ (cell_pair) =>
            match cell_pair
            | (Floor, Floor) => true
            | (Empty, Empty) => true
            | (Occupied, Occupied) => true
            else false
            end
          })
      })

  fun print_grid(grid: Grid) =>
    for row in grid.values() do
      _answer.answer(String.from_array(recover
        Iter[Cell](row.values())
          .map[U8]({ (cell) =>
            match cell
            | Floor => '.'
            | Empty => 'L'
            | Occupied => '#'
            end
          })
          .collect[Array[U8]](Array[U8])
      end))
    end
    _answer.answer("")
