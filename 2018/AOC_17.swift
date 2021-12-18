let spring = Position(x: 500, y: 0)

let inputParser = try! RegExp(pattern: "^([xy])=(\\d+), ([xy])=(\\d+)\\.\\.(\\d+)$")

struct Position: Hashable, Comparable, CustomStringConvertible {
    let x: Int
    let y: Int

    var description: String {
        return "(\(x), \(y))"
    }

    var above: Position {
        return Position(x: x, y: y - 1)
    }

    var below: Position {
        return Position(x: x, y: y + 1)
    }

    var left: Position {
        return Position(x: x - 1, y: y)
    }

    var right: Position {
        return Position(x: x + 1, y: y)
    }

    static func < (lhs: Position, rhs: Position) -> Bool {
        if lhs.y != rhs.y {
            return lhs.y < rhs.y
        } else {
            return lhs.x < rhs.x
        }
    }
}

struct Clay: Equatable {
    let tiles: Set<Position>
    let xRange: ClosedRange<Int>
    let yRange: ClosedRange<Int>

    init(tiles: Set<Position>) {
        self.tiles = tiles
        let xs = tiles.map { tile in tile.x }.sorted()
        xRange = (xs.first! - 3) ... (xs.last! + 3)
        let ys = tiles.map { tile in tile.y }.sorted()
        yRange = ys.first! ... ys.last!
    }

    func contains(_ tile: Position) -> Bool {
        return tiles.contains(tile)
    }

    func binds(_ tile: Position) -> Bool {
        return yRange.contains(tile.y)
    }

    func canStop(flowingWater tile: Position) -> Bool {
        return tile.y <= yRange.last!
    }
}

struct Ground: Equatable, CustomStringConvertible {
    typealias Water = Set<Position>

    let clay: Clay
    let water: Water
    let restingWater: Water
    let settledWater: Water

    var description: String {
        return description(until: clay.yRange.last!)
    }

    func description(until maxY: Int) -> String {
        return (0 ... min(clay.yRange.last!, maxY)).map { y in
            clay.xRange.map { x in
                let tile = Position(x: x, y: y)
                if settledWater.contains(tile) {
                    return "~"
                } else if restingWater.contains(tile) {
                    return "|"
                } else if water.contains(tile) {
                    return "|"
                } else if clay.tiles.contains(tile) {
                    return "#"
                } else {
                    return "."
                }
            }.joined() + "\n"
        }.joined()
    }

    init(spring: Position, clay: Clay) {
        self.init(clay: clay, water: [spring], restingWater: [], settledWater: [])
    }

    init(clay: Clay, water: Water, restingWater: Water, settledWater: Water) {
        self.clay = clay
        self.water = water
        self.restingWater = restingWater
        self.settledWater = settledWater
    }

    var tilesReachedByWater: Int {
        return water.union(restingWater).union(settledWater).filter(clay.binds).count
    }

    func flow() -> Ground {
        var newWater: Water = []
        var newRestingWater: Water = []
        var newSettledWater = settledWater

        let isSettling = { (tile: Position) -> Bool in
            self.clay.contains(tile.below) || newSettledWater.contains(tile.below)
        }

        for tile in water {
            if isSettling(tile) {
                var flowingWater: Water = [tile]
                var overflowingWater: Water = []
                var current: Position

                current = tile
                while !clay.contains(current) {
                    if !isSettling(current) {
                        overflowingWater.insert(current)
                        break
                    }
                    flowingWater.insert(current)
                    current = current.left
                }

                current = tile
                while !clay.contains(current) {
                    if !isSettling(current) {
                        overflowingWater.insert(current)
                        break
                    }
                    flowingWater.insert(current)
                    current = current.right
                }

                if overflowingWater.isEmpty {
                    newWater.insert(tile.above)
                    newSettledWater.formUnion(flowingWater)
                } else {
                    newRestingWater.formUnion(flowingWater)
                    newWater.formUnion(overflowingWater)
                }
            } else if clay.canStop(flowingWater: tile.below) {
                newWater.insert(tile.below)
            }
        }

        return Ground(
            clay: clay,
            water: newWater.subtracting(newSettledWater),
            restingWater: newRestingWater.union(water).union(restingWater),
            settledWater: newSettledWater
        )
    }
}

func main() {
    let clay = Clay(tiles: StdIn().map(parseInput).reduce(Set()) { a, b in a.union(b) })
    var ground = Ground(spring: spring, clay: clay)
    while true {
        let nextGround = ground.flow()
        if nextGround == ground {
            break
        }
        ground = nextGround
    }

    print(ground)
    print("Tiles reached by water:", ground.tilesReachedByWater)
    print("Water tiles left after the spring runs dry:", ground.settledWater.count)
}

func parseInput(line: String) -> Set<Position> {
    guard let match = inputParser.firstMatch(in: line) else {
        fatalError("Could not parse input: \"\(line)\"")
    }
    if match[1] == "x" {
        let x = Int(match[2])!
        let ys = Int(match[4])! ... Int(match[5])!
        return Set(ys.map { y in Position(x: x, y: y) })
    } else {
        let y = Int(match[2])!
        let xs = Int(match[4])! ... Int(match[5])!
        return Set(xs.map { x in Position(x: x, y: y) })
    }
}
