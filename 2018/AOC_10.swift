let numberRegExp = "\\s*(-?\\d+)"
let parser = try! RegExp(pattern: "^position=<\(numberRegExp),\(numberRegExp)> velocity=<\(numberRegExp),\(numberRegExp)>$")

struct Position: Hashable {
    let x: Int
    let y: Int

    static func + (position: Position, velocity: Velocity) -> Position {
        return Position(x: position.x + velocity.x, y: position.y + velocity.y)
    }
}

struct Velocity: Hashable {
    let x: Int
    let y: Int
}

struct PointOfLight: Hashable {
    let position: Position
    let velocity: Velocity

    func move() -> PointOfLight {
        return PointOfLight(position: position + velocity, velocity: velocity)
    }
}

struct PointsOfLight: CustomStringConvertible {
    let points: [PointOfLight]

    var description: String {
        let positions = Set(points.map { point in point.position })
        let xs = positions.map { position in position.x }
        let ys = positions.map { position in position.y }

        return (ys.min()! ... ys.max()!).map { y in
            (xs.min()! ... xs.max()!).map { x in
                positions.contains(Position(x: x, y: y)) ? "#" : " "
            }.joined()
        }.joined(separator: "\n")
    }

    func move() -> PointsOfLight {
        return PointsOfLight(points: points.map { point in point.move() })
    }

    func aligned() -> Bool {
        let pointsByX = Dictionary(grouping: points.map { point in point.position }, by: { position in position.x })
            .mapValues { positions in Set(positions.map { position in position.y }).sorted() }
        var lineCount = 0
        for ys in pointsByX.values {
            var consecutiveCount = 1
            if var last = ys.first {
                for y in ys.dropFirst() {
                    if y == last + 1 {
                        consecutiveCount += 1
                        if consecutiveCount == 5 {
                            lineCount += 1
                            if lineCount == 3 {
                                return true
                            }
                            break
                        }
                    } else {
                        consecutiveCount = 1
                    }
                    last = y
                }
            }
        }
        return false
    }
}

func main() {
    var points = PointsOfLight(points: StdIn().map(parseLine))
    var count = 0
    while !points.aligned() {
        points = points.move()
        count += 1
    }
    print(points)
    print()
    print("The elves would have needed to wait for \(count) seconds.")
}

func parseLine(string: String) -> PointOfLight {
    guard let match = parser.firstMatch(in: string) else {
        fatalError("Could not parse line: \(string)")
    }
    return PointOfLight(
        position: Position(x: Int(match[1])!, y: Int(match[2])!),
        velocity: Velocity(x: Int(match[3])!, y: Int(match[4])!)
    )
}
