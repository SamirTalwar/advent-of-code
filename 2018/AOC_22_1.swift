typealias Depth = Int

struct Position: Hashable {
    let x: Int
    let y: Int
}

let depthParser = try! RegExp(pattern: "^depth: (\\d+)$")
let targetParser = try! RegExp(pattern: "^target: (\\d+),(\\d+)$")

let mouth = Position(x: 0, y: 0)

func main() {
    let depth = Depth(depthParser.firstMatch(in: readLine()!)![1])!
    let targetMatch = targetParser.firstMatch(in: readLine()!)!
    let target = Position(x: Int(targetMatch[1])!, y: Int(targetMatch[2])!)

    var erosionLevels: [Position: Int] = [:]

    func riskLevel(of region: Position) -> Int {
        return erosionLevel(of: region) % 3
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

    var totalRiskLevel = 0
    for y in mouth.y ... target.y {
        for x in mouth.x ... target.x {
            totalRiskLevel += riskLevel(of: Position(x: x, y: y))
        }
    }
    print(totalRiskLevel)
}
