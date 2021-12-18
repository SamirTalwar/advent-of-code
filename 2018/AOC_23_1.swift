let inputParser = try! RegExp(pattern: "^pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)$")

struct Position {
    let x: Int
    let y: Int
    let z: Int

    func distance(from other: Position) -> Int {
        return abs(x - other.x) + abs(y - other.y) + abs(z - other.z)
    }
}

struct Nanobot: CustomStringConvertible {
    let position: Position
    let signalRadius: Int

    var description: String {
        return "pos=<\(position.x),\(position.y),\(position.z)>, r=\(signalRadius)"
    }

    func isInRange(of bot: Nanobot) -> Bool {
        return position.distance(from: bot.position) <= bot.signalRadius
    }
}

func main() {
    let nanobots = StdIn().map(parseInput)
    let strongestNanobot = nanobots.max(by: comparing { bot in bot.signalRadius })!
    let inRange = nanobots.filter { bot in bot.isInRange(of: strongestNanobot) }
    print(inRange.count)
}

func parseInput(input: String) -> Nanobot {
    guard let match = inputParser.firstMatch(in: input) else {
        fatalError("Could not parse \"\(input)\".")
    }
    return Nanobot(
        position: Position(
            x: Int(match[1])!,
            y: Int(match[2])!,
            z: Int(match[3])!
        ),
        signalRadius: Int(match[4])!
    )
}
