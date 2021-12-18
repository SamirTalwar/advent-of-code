struct Claim {
    let id: Int
    let positions: Set<Position>

    init(id: Int, left: Int, top: Int, width: Int, height: Int) {
        self.id = id
        positions = Set((left ..< (left + width)).flatMap { x in (top ..< (top + height)).map { y in Position(x: x, y: y) } })
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
    guard let validClaim = claims.first(where: { claim in claim.positions.allSatisfy { position in claimed[position] == 1 } }) else {
        fatalError("There is no valid claim.")
    }
    print(validClaim.id)
}

let claimParser = try! RegExp(pattern: "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$")

func parseClaim(string: String) -> Claim {
    guard let match = claimParser.firstMatch(in: string) else {
        fatalError("This claim was unparseable: \(string)")
    }
    let extract = { Int(match[$0])! }
    return Claim(
        id: extract(1),
        left: extract(2),
        top: extract(3),
        width: extract(4),
        height: extract(5)
    )
}
