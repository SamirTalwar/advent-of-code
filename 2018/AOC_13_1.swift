enum Turning {
    case left
    case straight
    case right
}

enum Direction {
    case north
    case east
    case south
    case west

    var isHorizontal: Bool {
        return self == .east || self == .west
    }

    var isVertical: Bool {
        return self == .north || self == .south
    }

    func turn(_ turning: Turning) -> Direction {
        switch (self, turning) {
        case (.north, .left):
            return .west
        case (.east, .left):
            return .north
        case (.south, .left):
            return .east
        case (.west, .left):
            return .south
        case (.north, .right):
            return .east
        case (.east, .right):
            return .south
        case (.south, .right):
            return .west
        case (.west, .right):
            return .north
        case (_, .straight):
            return self
        }
    }
}

struct Position: Equatable {
    let x: Int
    let y: Int

    func move(_ direction: Direction) -> Position {
        switch direction {
        case .north:
            return Position(x: x, y: y - 1)
        case .east:
            return Position(x: x + 1, y: y)
        case .south:
            return Position(x: x, y: y + 1)
        case .west:
            return Position(x: x - 1, y: y)
        }
    }
}

enum TrackPiece {
    case empty
    case horizontal
    case vertical
    case turningLeftFromNorthOrSouth
    case turningRightFromNorthOrSouth
    case intersection
}

extension TrackPiece: CustomStringConvertible {
    var description: String {
        switch self {
        case .empty:
            return " "
        case .horizontal:
            return "-"
        case .vertical:
            return "|"
        case .turningLeftFromNorthOrSouth:
            return "\\"
        case .turningRightFromNorthOrSouth:
            return "/"
        case .intersection:
            return "+"
        }
    }
}

struct Tracks: CustomStringConvertible {
    private let tracks: [[TrackPiece]]

    init(_ tracks: [[TrackPiece]]) {
        self.tracks = tracks
    }

    var description: String {
        return tracks.map { row in row.map { cell in cell.description }.joined() }.joined(separator: "\n")
    }

    subscript(position: Position) -> TrackPiece {
        return tracks[position.y][position.x]
    }
}

struct Cart {
    let position: Position
    let direction: Direction
    let intersectionTurning: Turning
}

struct Crash {
    let position: Position
}

func main() {
    let tracksAndCarts = StdIn().enumerated().map { line in
        line.element.enumerated().map { character in
            parseTrack(position: Position(x: character.offset, y: line.offset), character: character.element)
        }
    }
    let columns = tracksAndCarts.map { row in row.count }.max()!
    let tracks = Tracks(tracksAndCarts.map { row in
        pad(row.map { track, _ in track }, to: columns, with: .empty)
    })
    let initialCarts = tracksAndCarts.flatMap { row in row.compactMap { _, cart in cart } }

    var carts = initialCarts
    var crashes: [Crash] = []
    while crashes.isEmpty {
        carts = tick(tracks: tracks, carts: carts)
        crashes = findCrashes(carts: carts)
    }

    print(crashes)
}

func tick(tracks: Tracks, carts: [Cart]) -> [Cart] {
    return carts.map { cart in
        let newPosition = cart.position.move(cart.direction)
        var direction = cart.direction
        var intersectionTurning = cart.intersectionTurning
        switch tracks[newPosition] {
        case .empty:
            fatalError("A cart ended up on a piece of empty track.")
        case .horizontal:
            if cart.direction.isVertical {
                fatalError("A cart ended up going vertically on a horizontal track.")
            }
        case .vertical:
            if cart.direction.isHorizontal {
                fatalError("A cart ended up going horizontally on a vertical track.")
            }
        case .turningLeftFromNorthOrSouth:
            direction = cart.direction.isVertical ? cart.direction.turn(.left) : cart.direction.turn(.right)
        case .turningRightFromNorthOrSouth:
            direction = cart.direction.isVertical ? cart.direction.turn(.right) : cart.direction.turn(.left)
        case .intersection:
            direction = direction.turn(intersectionTurning)
            switch intersectionTurning {
            case .left:
                intersectionTurning = .straight
            case .straight:
                intersectionTurning = .right
            case .right:
                intersectionTurning = .left
            }
        }
        return Cart(position: newPosition, direction: direction, intersectionTurning: intersectionTurning)
    }
}

func findCrashes(carts: [Cart]) -> [Crash] {
    var crashes: [Crash] = []
    for i in 0 ..< (carts.count - 1) {
        for j in (i + 1) ..< carts.count {
            if carts[i].position == carts[j].position {
                crashes.append(Crash(position: carts[i].position))
            }
        }
    }
    return crashes
}

func parseTrack(position: Position, character: Character) -> (TrackPiece, Cart?) {
    switch character {
    case " ":
        return (.empty, nil)
    case "-":
        return (.horizontal, nil)
    case "|":
        return (.vertical, nil)
    case "\\":
        return (.turningLeftFromNorthOrSouth, nil)
    case "/":
        return (.turningRightFromNorthOrSouth, nil)
    case "+":
        return (.intersection, nil)
    case "^":
        return (.vertical, Cart(position: position, direction: .north, intersectionTurning: .left))
    case "v":
        return (.vertical, Cart(position: position, direction: .south, intersectionTurning: .left))
    case "<":
        return (.horizontal, Cart(position: position, direction: .west, intersectionTurning: .left))
    case ">":
        return (.horizontal, Cart(position: position, direction: .east, intersectionTurning: .left))
    default:
        preconditionFailure("\"\(character)\" is an invalid track piece.")
    }
}

func pad<T>(_ array: [T], to size: Int, with value: T) -> [T] {
    if array.count < size {
        return array + Array(repeating: value, count: size - array.count)
    }
    return array
}
