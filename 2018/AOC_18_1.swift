enum Acre: CustomStringConvertible {
    case openGround
    case trees
    case lumberyard

    var description: String {
        switch self {
        case .openGround:
            return "."
        case .trees:
            return "|"
        case .lumberyard:
            return "#"
        }
    }

    func convert(adjacentCounts: [Acre: Int]) -> Acre {
        switch self {
        case .openGround:
            return (adjacentCounts[.trees] ?? 0) >= 3 ? .trees : self
        case .trees:
            return (adjacentCounts[.lumberyard] ?? 0) >= 3 ? .lumberyard : self
        case .lumberyard:
            return (adjacentCounts[.lumberyard] ?? 0) >= 1 && (adjacentCounts[.trees] ?? 0) >= 1 ? self : .openGround
        }
    }
}

struct Grid: CustomStringConvertible {
    let values: [[Acre]]
    let xRange: Range<Int>
    let yRange: Range<Int>

    init(_ values: [[Acre]]) {
        self.values = values
        xRange = 0 ..< values[0].count
        yRange = 0 ..< values.count
    }

    var description: String {
        return values.map({ row in row.map({ acre in acre.description }).joined() + "\n" }).joined()
    }

    func count(of acreType: Acre) -> Int {
        return values.flatMap({ row in row.filter({ acre in acre == acreType }) }).count
    }

    func iterate() -> Grid {
        return Grid(yRange.map({ y in
            xRange.map({ x in
                values[y][x].convert(adjacentCounts: adjacentValueCounts(x: x, y: y))
            })
        }))
    }

    private func adjacentValueCounts(x: Int, y: Int) -> [Acre: Int] {
        let adjacentPositions = [
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
            (x - 1, y),
            (x + 1, y),
            (x - 1, y + 1),
            (x, y + 1),
            (x + 1, y + 1),
        ]
        let adjacentValues = adjacentPositions
            .filter({ x, y in xRange.contains(x) && yRange.contains(y) })
            .map({ x, y in values[y][x] })

        var counts: [Acre: Int] = [:]
        for value in adjacentValues {
            if let count = counts[value] {
                counts[value] = count + 1
            } else {
                counts[value] = 1
            }
        }
        return counts
    }
}

func main() {
    var grid = Grid(StdIn().map({ line in line.map(parseInputCharacter) }))
    for _ in 1 ... 10 {
        grid = grid.iterate()
    }
    print(grid)
    print(grid.count(of: .trees) * grid.count(of: .lumberyard))
}

func parseInputCharacter(_ character: Character) -> Acre {
    switch character {
    case ".":
        return .openGround
    case "|":
        return .trees
    case "#":
        return .lumberyard
    default:
        fatalError("Could not parse \"\(character)\".")
    }
}
