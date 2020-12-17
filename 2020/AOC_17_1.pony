use collections = "collections"
use "collections/persistent"
use "itertools"

primitive Active
primitive Inactive
type Cell is (Inactive | Active)
type Row is Vec[Cell]
type Grid is Vec[Row]

class val Coordinates is (Equatable[Coordinates] & collections.Hashable)
  let x: ISize
  let y: ISize
  let z: ISize

  new val create(x': ISize, y': ISize, z': ISize) =>
    x = x'
    y = y'
    z = z'

  fun val neighbors(): Iterator[Coordinates] =>
    Neighbors(this)

  fun eq(that: box->Coordinates): Bool =>
    (this.x == that.x) and (this.y == that.y) and (this.z == that.z)

  fun hash(): USize =>
    var result: USize = 17
    result = (result * 31) + x.hash()
    result = (result * 31) + y.hash()
    result = (result * 31) + z.hash()
    result

  fun string(): String =>
    "(" + x.string() + ", " + y.string() + ", " + z.string() + ")"

class Neighbors is Iterator[Coordinates]
  let _coordinates: Coordinates
  var _state: ISize = 0

  new create(coordinates: Coordinates) =>
    _coordinates = coordinates

  fun ref has_next(): Bool =>
    _state < (3 * 3 * 3)

  fun ref next(): Coordinates ? =>
    if _state >= (3 * 3 * 3) then
      error
    end

    let x = (_state % 3) - 1
    let y = ((_state / 3) % 3) - 1
    let z = (((_state / 3) / 3) % 3) - 1
    _state = _state + 1
    if _state == ((3 * 3 * 3) / 2) then
      _state = _state + 1
    end
    Coordinates(_coordinates.x + x, _coordinates.y + y, _coordinates.z + z)

type ActiveCells is Map[Coordinates, Cell]

actor Main
  new create(env: Env) =>
    let orchestrator = Orchestrator(env)
    let collector = GridCollector[Cell](orchestrator, Parser)
    let solution = Solution(orchestrator)
    orchestrator.start[Grid](collector, solution)

class Parser is CharacterParser[Cell]
  fun parse(character: U8): Cell ? =>
    match character
    | '.' => Inactive
    | '#' => Active
    else error
  end

actor Solution is Solve[Grid]
  let _answer: (Answer tag & Escape tag)

  new create(answer: (Answer tag & Escape tag)) =>
    _answer = answer

  be apply(grid: Grid) =>
    var active_cells = ActiveCells
    for (y, row) in grid.pairs() do
      for (x, cell) in row.pairs() do
        if cell is Active then
          active_cells = (active_cells(Coordinates(x.isize(), y.isize(), 0)) = cell)
        end
      end
    end

    cycle(active_cells, 6)

  be cycle(active_cells: ActiveCells, cycles_remaining: USize) =>
    if cycles_remaining == 0 then
      _answer.answer(active_cells.size())
      return
    end

    let candidates = ToSet[Coordinates].from_iter(
      Iter[Coordinates](active_cells.keys())
      .flat_map[Coordinates]({ (coordinates) =>
        Iter[Coordinates].chain([Iter[Coordinates].maybe(coordinates); coordinates.neighbors()].values())
      }))

    var new_active_cells = ActiveCells
    for candidate in candidates.values() do
      var neighbor_count: USize = 0
      for neighbor in candidate.neighbors() do
        if active_cells.get_or_else(neighbor, Inactive) is Active then
          neighbor_count = neighbor_count + 1
        end
      end
      let new_cell = match (active_cells.get_or_else(candidate, Inactive), neighbor_count)
      | (Active, 2) => Active
      | (Active, 3) => Active
      | (Inactive, 3) => Active
      else Inactive
      end
      if new_cell is Active then
        new_active_cells = (new_active_cells(candidate) = Active)
      end
    end

    cycle(new_active_cells, cycles_remaining - 1)
