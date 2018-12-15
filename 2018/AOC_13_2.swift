enum Turning {
    case left
    case straight
    case right
}

enum Direction: CustomStringConvertible {
    case north
    case east
    case south
    case west

    var description: String {
        switch self {
        case .north:
            return "^"
        case .east:
            return ">"
        case .south:
            return "v"
        case .west:
            return "<"
        }
    }

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
        return tracks.map({ row in row.map({ cell in cell.description }).joined() }).joined(separator: "\n")
    }

    subscript(position: Position) -> TrackPiece {
        return tracks[position.y][position.x]
    }
}

struct Cart: CustomStringConvertible {
    let position: Position
    let direction: Direction
    let intersectionTurning: Turning

    var description: String {
        return "\(position) \(direction)"
    }

    func move(tracks: Tracks) -> Cart {
        let newPosition = position.move(direction)
        var newDirection = direction
        var newIntersectionTurning = intersectionTurning
        switch tracks[newPosition] {
        case .empty:
            fatalError("A cart ended up on a piece of empty track.")
        case .horizontal:
            if direction.isVertical {
                fatalError("A cart ended up going vertically on a horizontal track.")
            }
        case .vertical:
            if direction.isHorizontal {
                fatalError("A cart ended up going horizontally on a vertical track.")
            }
        case .turningLeftFromNorthOrSouth:
            newDirection = direction.isVertical ? direction.turn(.left) : direction.turn(.right)
        case .turningRightFromNorthOrSouth:
            newDirection = direction.isVertical ? direction.turn(.right) : direction.turn(.left)
        case .intersection:
            newDirection = direction.turn(intersectionTurning)
            switch intersectionTurning {
            case .left:
                newIntersectionTurning = .straight
            case .straight:
                newIntersectionTurning = .right
            case .right:
                newIntersectionTurning = .left
            }
        }
        return Cart(
            position: newPosition,
            direction: newDirection,
            intersectionTurning: newIntersectionTurning
        )
    }
}

func main() {
    let tracksAndCarts = StdIn().enumerated().map({ line in
        line.element.enumerated().map({ character in
            parseTrack(position: Position(x: character.offset, y: line.offset), character: character.element)
        })
    })
    let columns = tracksAndCarts.map({ row in row.count }).max()!
    let tracks = Tracks(tracksAndCarts.map({ row in
        pad(row.map({ track, _ in track }), to: columns, with: .empty)
    }))
    let initialCarts = tracksAndCarts.flatMap({ row in row.compactMap({ _, cart in cart }) })

    var carts = initialCarts
    while carts.count > 1 {
        carts.sort(by: comparing({ cart in cart.position }))
        var i = 0
        while carts.count > 1 && i < carts.count {
            carts[i] = carts[i].move(tracks: tracks)
            var crash = false
            for crashIndex in carts.startIndex ..< carts.endIndex {
                if crashIndex >= carts.endIndex {
                    break
                }
                if crashIndex != i && carts[crashIndex].position == carts[i].position {
                    crash = true
                    carts.remove(at: crashIndex)
                    if crashIndex < i {
                        i -= 1
                    }
                }
            }

            if crash {
                carts.remove(at: i)
            } else {
                i += 1
            }
        }
    }

    carts[0] = carts[0].move(tracks: tracks)
    print(carts[0].position)
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
