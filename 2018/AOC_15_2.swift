let startingHitPoints = 200

typealias HitPoints = Int

typealias Route = [Position]

struct Position: Equatable, Comparable, Hashable, CustomStringConvertible {
    let x: Int
    let y: Int

    var description: String {
        return "(\(x), \(y))"
    }

    static func < (lhs: Position, rhs: Position) -> Bool {
        if lhs.y != rhs.y {
            return lhs.y < rhs.y
        } else {
            return lhs.x < rhs.x
        }
    }

    func distance(from other: Position) -> Int {
        return abs(x - other.x) + abs(y - other.y)
    }

    var neighbors: [Position] {
        return [
            Position(x: x, y: y - 1),
            Position(x: x - 1, y: y),
            Position(x: x + 1, y: y),
            Position(x: x, y: y + 1),
        ]
    }
}

enum CaveSquare {
    case openCavern
    case wall
}

extension CaveSquare: CustomStringConvertible {
    var description: String {
        switch self {
        case .openCavern:
            return "."
        case .wall:
            return "#"
        }
    }
}

struct Cave: CustomStringConvertible {
    private let cave: [[CaveSquare]]

    init(_ cave: [[CaveSquare]]) {
        self.cave = cave
    }

    var description: String {
        return cave.map { row in row.map { cell in cell.description }.joined() }.joined(separator: "\n")
    }

    func description(with warriors: [Warrior]) -> String {
        let warriorsByPosition = Dictionary(uniqueKeysWithValues: warriors.map { warrior in (warrior.position, warrior) })
        return cave.enumerated().map { row in
            row.element.enumerated().map { cell in
                warriorsByPosition[Position(x: cell.offset, y: row.offset)]?.race.symbol ?? cell.element.description
            }.joined() + "\n"
        }.joined()
    }

    subscript(position: Position) -> CaveSquare {
        return cave[position.y][position.x]
    }

    func route(from start: Position, to end: Position, excluding blocked: Set<Position>) -> Route? {
        if blocked.contains(end) || !isOpen(position: end) {
            return nil
        }
        return BreadthFirstSearch<Position>(
            neighbors: neighbors(excluding: blocked)
        ).shortestPath(from: start, to: end)
    }

    private func neighbors(excluding blocked: Set<Position>) -> (Position) -> [Position] {
        return { position in
            position.neighbors.filter { neighbor in
                !blocked.contains(neighbor) && self.isOpen(position: neighbor)
            }
        }
    }

    private func isOpen(position: Position) -> Bool {
        return position.y >= 0 && position.y < cave.count
            && position.x >= 0 && position.x < cave[0].count
            && cave[position.y][position.x] == .openCavern
    }
}

enum Race: CustomStringConvertible {
    case elf
    case goblin

    var description: String {
        switch self {
        case .elf:
            return "Elf"
        case .goblin:
            return "Goblin"
        }
    }

    var symbol: String {
        switch self {
        case .elf:
            return "E"
        case .goblin:
            return "G"
        }
    }
}

struct Warrior {
    let race: Race
    let position: Position
    var hitPoints: HitPoints

    func isInRange(of aggressor: Warrior) -> Bool {
        return aggressor.position.distance(from: position) <= 1
    }

    func findTargets(in warriors: [Warrior]) -> [(offset: Int, element: Warrior)] {
        return warriors.enumerated()
            .filter { warrior in warrior.element.race != race }
            .sorted(by: comparing { target in target.element.position })
            .sorted(by: comparing { target in target.element.hitPoints })
    }

    func move(along route: Route) -> Warrior {
        return Warrior(race: race, position: route.first!, hitPoints: hitPoints)
    }
}

enum AttackError: Error {
    case elfDied
}

func main() {
    let caveSquaresAndWarriors = StdIn().enumerated().map { line in
        line.element.enumerated().map { character in
            parseCave(position: Position(x: character.offset, y: line.offset), character: character.element)
        }
    }
    let cave = Cave(caveSquaresAndWarriors.map { row in
        row.map { caveSquare, _ in caveSquare }
    })

    var attackPower = [
        Race.elf: 3,
        Race.goblin: 3,
    ]
    while true {
        do {
            var warriors = caveSquaresAndWarriors.flatMap { row in row.compactMap { _, warrior in warrior } }

            // print(cave.description(with: warriors))

            func attack(attackPower: Int, offset: Int, i: inout Int) throws {
                warriors[offset].hitPoints -= attackPower
                if warriors[offset].hitPoints <= 0 {
                    if warriors[offset].race == .elf {
                        throw AttackError.elfDied
                    }
                    warriors.remove(at: offset)
                    if offset < i {
                        i -= 1
                    }
                }
            }

            var rounds = 0
            while true {
                warriors.sort(by: comparing { warriors in warriors.position })
                var i = 0
                var roundWasCompleted = true
                while i < warriors.count {
                    let targets = warriors[i].findTargets(in: warriors)
                    if targets.isEmpty {
                        roundWasCompleted = false
                        break
                    }

                    if let target = targets.first(where: { target in target.element.isInRange(of: warriors[i]) }) {
                        try attack(attackPower: attackPower[warriors[i].race]!, offset: target.offset, i: &i)
                    } else {
                        let blocked = Set(warriors.map { warrior in warrior.position })
                        let optimisticDistances = Set(targets.flatMap { target in target.element.position.neighbors })
                            .map { position in (position, position.distance(from: warriors[i].position)) }
                            .sorted(by: comparing { position, _ in position })
                            .sorted(by: comparing { _, distance in distance })
                        var bestRoute: (Position, Route)?
                        for (position, optimisticDistance) in optimisticDistances {
                            if let (_, chosenRoute) = bestRoute {
                                if optimisticDistance > chosenRoute.count {
                                    break
                                }
                            }
                            if let route = cave.route(from: warriors[i].position, to: position, excluding: blocked) {
                                switch bestRoute {
                                case .none:
                                    bestRoute = (position, route)
                                case let .some(chosenPosition, chosenRoute):
                                    if route.count < chosenRoute.count || (route.count == chosenRoute.count && position < chosenPosition) {
                                        bestRoute = (position, route)
                                    }
                                }
                            }
                        }
                        if let (_, route) = bestRoute {
                            warriors[i] = warriors[i].move(along: route)
                            let targets = warriors[i].findTargets(in: warriors)
                            if let target = targets.first(where: { target in target.element.isInRange(of: warriors[i]) }) {
                                try attack(attackPower: attackPower[warriors[i].race]!, offset: target.offset, i: &i)
                            }
                        }
                    }

                    i += 1
                }

                if !roundWasCompleted {
                    // print("Round \(rounds) was not completed.")
                    // for warrior in warriors.sorted(by: comparing({ warriors in warriors.position })) {
                    //     print(warrior)
                    // }
                    // print(cave.description(with: warriors))
                    break
                }

                rounds += 1
                // print(rounds)
                // for warrior in warriors.sorted(by: comparing({ warriors in warriors.position })) {
                //     print(warrior)
                // }
                // print(cave.description(with: warriors))
            }

            let totalHitPoints = warriors.map { warrior in warrior.hitPoints }.reduce(0, +)
            print("Elf Attack Power = \(attackPower[.elf]!)")
            print("Outcome = \(rounds) * \(totalHitPoints) = \(rounds * totalHitPoints)")
            break
        } catch AttackError.elfDied {
            attackPower[.elf]! += 1
        } catch {
            fatalError("\(error)")
        }
    }
}

func parseCave(position: Position, character: Character) -> (CaveSquare, Warrior?) {
    switch character {
    case ".":
        return (.openCavern, nil)
    case "#":
        return (.wall, nil)
    case "E":
        return (.openCavern, Warrior(race: .elf, position: position, hitPoints: startingHitPoints))
    case "G":
        return (.openCavern, Warrior(race: .goblin, position: position, hitPoints: startingHitPoints))
    default:
        preconditionFailure("\"\(character)\" is an invalid cave square.")
    }
}
