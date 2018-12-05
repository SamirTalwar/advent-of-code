import Foundation

struct Claim {
    let id: Int
    let positions: Set<Position>

    init(id: Int, left: Int, top: Int, width: Int, height: Int) {
        self.id = id
        positions = Set((left ..< (left + width)).flatMap { x in (top ..< (top + height)).map { y in (Position(x: x, y: y)) } })
    }
}

struct Position: Hashable {
    let x: Int
    let y: Int
}

func main() {
    let claims = StdIn().map(parseClaim)
    var claimed: [Position: Int] = [:]
    for claim in claims {
        for position in claim.positions {
            claimed[position] = (claimed[position] ?? 0) + 1
        }
    }
    let claimedByMany = claimed.lazy.filter { _, count in count > 1 }
    print(claimedByMany.count)
}

let claimParser = try! NSRegularExpression(pattern: "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$")

func parseClaim(string: String) -> Claim {
    guard let match = claimParser.firstMatch(in: string, range: NSMakeRange(0, string.count)) else {
        fatalError("This claim was unparseable: \(string)")
    }
    let extract = { Int(string[Range(match.range(at: $0), in: string)!])! }
    return Claim(
        id: extract(1),
        left: extract(2),
        top: extract(3),
        width: extract(4),
        height: extract(5)
    )
}
