typealias Distance = Int

struct Room: Hashable, CustomStringConvertible {
    let x: Int
    let y: Int

    var description: String {
        return "(\(x), \(y))"
    }
}

enum Direction: CustomStringConvertible {
    case north
    case east
    case south
    case west

    var description: String {
        switch self {
        case .north:
            return "N"
        case .east:
            return "E"
        case .south:
            return "S"
        case .west:
            return "W"
        }
    }

    var opposite: Direction {
        switch self {
        case .north:
            return .south
        case .east:
            return .west
        case .south:
            return .north
        case .west:
            return .east
        }
    }

    func move(from room: Room) -> Room {
        switch self {
        case .north:
            return Room(x: room.x, y: room.y - 1)
        case .east:
            return Room(x: room.x + 1, y: room.y)
        case .south:
            return Room(x: room.x, y: room.y + 1)
        case .west:
            return Room(x: room.x - 1, y: room.y)
        }
    }
}

enum Directions: CustomStringConvertible {
    case move(direction: Direction)
    case sequence(parts: Array<Directions>.SubSequence)
    case choice(choices: [Directions])

    var description: String {
        switch self {
        case let .move(direction):
            return direction.description
        case let .sequence(parts):
            return "[" + parts.map({ $0.description }).joined() + "]"
        case let .choice(choices):
            return "(" + choices.map({ $0.description }).joined(separator: "|") + ")"
        }
    }

    var isEmpty: Bool {
        switch self {
        case .move:
            return false
        case let .sequence(parts):
            return parts.isEmpty
        case let .choice(choices):
            return choices.isEmpty
        }
    }

    func simplified() -> Directions {
        switch self {
        case .move:
            return self
        case let .sequence(parts):
            var simplifiedParts = parts.map({ $0.simplified() }).filter({ !$0.isEmpty })
            if simplifiedParts.count == 1 {
                return simplifiedParts[0]
            }
            return .sequence(parts: simplifiedParts[0 ..< simplifiedParts.count])
        case let .choice(choices):
            let simplifiedChoices = choices.map({ $0.simplified() })
            if simplifiedChoices.count == 1 {
                return simplifiedChoices[0]
            }
            return .choice(choices: simplifiedChoices)
        }
    }
}

func main() {
    let directions = parseInput(readLine()!)
    var distances: [Room: Distance] = [:]
    _ = roomDistances(directions: directions, room: Room(x: 0, y: 0), currentDistance: 0, record: &distances)
    print("Part 1:", distances.values.max()!)
    print("Part 2:", distances.values.filter({ $0 >= 1000 }).count)
}

func roomDistances(directions: Directions, room: Room, currentDistance: Distance, record: inout [Room: Distance]) -> Set<Room> {
    switch directions {
    case let .move(direction):
        let nextRoom = direction.move(from: room)
        if let knownDistance = record[nextRoom] {
            if knownDistance > currentDistance {
                record[nextRoom] = currentDistance + 1
            }
        } else {
            record[nextRoom] = currentDistance + 1
        }
        return [nextRoom]
    case let .sequence(parts):
        guard let first = parts.first else {
            return [room]
        }
        let nextRooms = roomDistances(directions: first, room: room, currentDistance: currentDistance, record: &record)
        let rest = parts.dropFirst()
        let directions = rest.count == 1 ? rest.first! : Directions.sequence(parts: rest)
        return Set(nextRooms.flatMap({ nextRoom in
            roomDistances(directions: directions, room: nextRoom, currentDistance: record[nextRoom]!, record: &record)
        })).filter({ furtherRoom in record[furtherRoom] == nil || record[furtherRoom]! >= currentDistance })
    case let .choice(choices):
        return Set(choices.flatMap({ choice in
            roomDistances(directions: choice, room: room, currentDistance: currentDistance, record: &record)
        })).filter({ furtherRoom in record[furtherRoom] == nil || record[furtherRoom]! >= currentDistance })
    }
}

func merged(_ roomsAndDistances: [([Room], [Room: Distance])]) -> ([Room], [Room: Distance]) {
    return roomsAndDistances.reduce(([], [:]), { a, b in (a.0 + b.0, a.1.merging(b.1, uniquingKeysWith: min)) })
}

func parseInput(_ string: String) -> Directions {
    if string.first! != "^" || string.last! != "$" {
        fatalError("Could not understand the directions.")
    }
    let (parsed, remaining) = parseDirections(string.dropFirst().dropLast())
    if !remaining.isEmpty {
        fatalError("Did not parse \"\(remaining)\".")
    }
    return parsed.simplified()
}

func parseDirections(_ string: Substring) -> (Directions, Substring) {
    var remaining = string
    var choices: [Directions] = []
    var parts: [Directions] = []
    while let character = remaining.first {
        remaining = remaining.dropFirst()
        if character == "(" {
            let (parsed, innerRemaining) = parseDirections(remaining)
            remaining = innerRemaining
            parts.append(parsed)
        } else if character == ")" {
            choices.append(Directions.sequence(parts: parts[0 ..< parts.count]))
            return (Directions.choice(choices: choices), remaining)
        } else if character == "|" {
            choices.append(Directions.sequence(parts: parts[0 ..< parts.count]))
            parts = []
        } else {
            parts.append(Directions.move(direction: parseDirection(character)))
        }
    }
    choices.append(Directions.sequence(parts: parts[0 ..< parts.count]))
    return (Directions.choice(choices: choices), remaining)
}

func parseDirection(_ character: Character) -> Direction {
    switch character {
    case "N":
        return .north
    case "E":
        return .east
    case "S":
        return .south
    case "W":
        return .west
    default:
        fatalError("Could not parse the direction \"\(character)\".")
    }
}
