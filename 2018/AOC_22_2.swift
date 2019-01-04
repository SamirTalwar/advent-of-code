typealias Depth = Int

struct Position: Hashable, CustomStringConvertible {
    let x: Int
    let y: Int

    var description: String {
        return "(\(x), \(y))"
    }

    var isValid: Bool {
        return x >= 0 && y >= 0
    }

    var neighbors: [Position] {
        return [
            Position(x: x + 1, y: y),
            Position(x: x, y: y + 1),
            Position(x: x - 1, y: y),
            Position(x: x, y: y - 1),
        ].filter({ position in position.isValid })
    }

    func distance(from other: Position) -> Int {
        return abs(x - other.x) + abs(y - other.y)
    }
}

enum RegionType {
    case rocky
    case wet
    case narrow

    var equipment: [Equipment] {
        switch self {
        case .rocky:
            return [.climbingGear, .torch]
        case .wet:
            return [.nothing, .climbingGear]
        case .narrow:
            return [.nothing, .torch]
        }
    }
}

enum Equipment: Hashable, CustomStringConvertible {
    case nothing
    case climbingGear
    case torch

    var description: String {
        switch self {
        case .nothing:
            return "nothing"
        case .climbingGear:
            return "climbing gear"
        case .torch:
            return "torch"
        }
    }
}

struct Traversal: Hashable, CustomStringConvertible {
    let position: Position
    let equipped: Equipment

    var description: String {
        return "\(position) w/ \(equipped)"
    }
}

let depthParser = try! RegExp(pattern: "^depth: (\\d+)$")
let targetParser = try! RegExp(pattern: "^target: (\\d+),(\\d+)$")

let mouth = Position(x: 0, y: 0)

func main() {
    let depth = Depth(depthParser.firstMatch(in: readLine()!)![1])!
    let targetMatch = targetParser.firstMatch(in: readLine()!)!
    let target = Position(x: Int(targetMatch[1])!, y: Int(targetMatch[2])!)

    var erosionLevels: [Position: Int] = [:]

    func regionType(of region: Position) -> RegionType {
        switch erosionLevel(of: region) % 3 {
        case 0:
            return .rocky
        case 1:
            return .wet
        case 2:
            return .narrow
        default:
            fatalError("Somehow n % 3 is not 0, 1 or 2.")
        }
    }

    func erosionLevel(of region: Position) -> Int {
        if let result = erosionLevels[region] {
            return result
        }
        let result = (geologicIndex(of: region) + depth) % 20183
        erosionLevels[region] = result
        return result
    }

    func geologicIndex(of region: Position) -> Int {
        if region == mouth || region == target {
            return 0
        }
        if region.y == 0 {
            return region.x * 16807
        }
        if region.x == 0 {
            return region.y * 48271
        }
        let left = erosionLevel(of: Position(x: region.x - 1, y: region.y))
        let top = erosionLevel(of: Position(x: region.x, y: region.y - 1))
        return left * top
    }

    func neighbors(of traversal: Traversal) -> [(Traversal, Cost)] {
        let adjacent = traversal.position.neighbors
            .map({ position in Traversal(position: position, equipped: traversal.equipped) })
            .filter({ traversal in regionType(of: traversal.position).equipment.contains(traversal.equipped) })
            .map({ traversal in (traversal, 1) })
        let switchEquipment = regionType(of: traversal.position).equipment
            .map({ equipment in (Traversal(position: traversal.position, equipped: equipment), 7) })
        return adjacent + switchEquipment
    }

    func costEstimate(from start: Traversal, to end: Traversal) -> Cost {
        return end.position.distance(from: start.position)
    }

    guard let path = AStar<Traversal>(
        neighbors: neighbors,
        costEstimate: costEstimate
    ).shortestPath(from: Traversal(position: mouth, equipped: .torch), to: Traversal(position: target, equipped: .torch)) else {
        fatalError("Could not find the path.")
    }

    print(path.cost)
}
