use collections = "collections"
use "collections/persistent"
use "itertools"

primitive Floor
primitive Empty
primitive Occupied
type Cell is (Floor | Empty | Occupied)

type Row is Vec[Cell]
type Grid is Vec[Row]

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = GridCollector[Cell](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Grid](collector, solution)

class Parser is CharacterParser[Cell]
  fun parse(character: U8): Cell ? =>
    match character
    | '.' => Floor
    | 'L' => Empty
    | '#' => Occupied
    else error
    end

actor Solution is Solve[Grid]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(grid: Grid) =>
    try
      let new_grid = step(grid)?
      if grids_equal(grid, new_grid) then
        _answer.answer(count_occupied(grid)?)
      else
        apply(new_grid)
      end
    else
      _answer.fail("Failed.")
    end

  fun step(grid: Grid): Grid ? =>
    var new_grid = Grid
    for y in collections.Range(0, grid.size()) do
      let row = grid(y)?
      var new_row = Row
      for x in collections.Range(0, row.size()) do
        let cell = row(x)?
        match cell
        | Floor =>
          new_row = new_row.push(cell)
        else
          let neighbor_count = count_neighbors(x, y, grid)?
          let new_cell = match neighbor_count
          | 0 => Occupied
          | let count: USize if count >= 4 => Empty
          else cell
          end
          new_row = new_row.push(new_cell)
        end
      end
      new_grid = new_grid.push(new_row)
    end
    new_grid

  fun count_neighbors(x: USize, y: USize, grid: Grid): USize ? =>
    var count: USize = 0
    for j in collections.Range(if y == 0 then 0 else y - 1 end, (y + 2).min(grid.size())) do
      let row = grid(j)?
      for i in collections.Range(if x == 0 then 0 else x - 1 end, (x + 2).min(row.size())) do
        if (i == x) and (j == y) then
          continue
        end
        let cell = row(i)?
        match cell
        | Occupied =>
          count = count + 1
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
