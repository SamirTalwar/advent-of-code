import Foundation

typealias Constellation = Int

struct Point: Hashable, CustomStringConvertible {
    let w: Int
    let x: Int
    let y: Int
    let z: Int

    var description: String {
        return "(\(w), \(x), \(y), \(z))"
    }

    func distance(from other: Point) -> Int {
        let wDistance: Int = abs(w - other.w)
        let xDistance: Int = abs(x - other.x)
        let yDistance: Int = abs(y - other.y)
        let zDistance: Int = abs(z - other.z)
        return wDistance + xDistance + yDistance + zDistance
    }
}

func main() {
    let points = Set(StdIn().map(parseInput))
    var constellations: [Constellation: Set<Point>] = [:]
    var nextConstellation = 1
    for point in points {
        var foundConstellations: Set<Constellation> = []
        for (constellation, others) in constellations {
            if others.contains(where: { other in point.distance(from: other) <= 3 }) {
                foundConstellations.insert(constellation)
            }
        }
        if let chosenConstellation = foundConstellations.popFirst() {
            constellations[chosenConstellation]!.insert(point)
            for otherConstellation in foundConstellations {
                constellations[chosenConstellation]!.formUnion(constellations[otherConstellation]!)
                constellations.removeValue(forKey: otherConstellation)
            }
        } else {
            constellations[nextConstellation] = [point]
            nextConstellation += 1
        }
    }

    print(constellations.count)
}

func parseInput(string: String) -> Point {
    let coordinates = string
        .components(separatedBy: ",")
        .map({ Int($0.trimmingCharacters(in: CharacterSet.whitespaces))! })
    return Point(
        w: coordinates[0],
        x: coordinates[1],
        y: coordinates[2],
        z: coordinates[3]
    )
}
