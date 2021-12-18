let inputParser = try! RegExp(pattern: "^pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)$")

let me = Position(x: 0, y: 0, z: 0)

struct Position: Hashable, CustomStringConvertible {
    let x: Int
    let y: Int
    let z: Int

    var description: String {
        return "(\(x), \(y), \(z))"
    }

    func distance(from other: Position) -> Int {
        return abs(x - other.x) + abs(y - other.y) + abs(z - other.z)
    }
}

struct Cube: Hashable {
    let center: Position
    let radius: Int

    func overlaps(with other: Cube) -> Bool {
        return other.center.distance(from: center) <= radius + other.radius
    }
}

struct Graph<T>: CustomStringConvertible where T: Hashable {
    private var edges: [T: Set<T>]

    init() {
        edges = [:]
    }

    var description: String {
        return edges.flatMap { a, bs in bs.map { b in "\(a) -> \(b)" } }.joined(separator: "\n")
    }

    mutating func addEdge(between a: T, and b: T) {
        if edges[a] == nil {
            edges[a] = []
        }
        if edges[b] == nil {
            edges[b] = []
        }
        edges[a]!.insert(b)
        edges[b]!.insert(a)
    }

    func maximalCliques() -> Set<Set<T>> {
        return bronKerbosch(r: [], p: Set(edges.keys), x: [])
    }

    private func bronKerbosch(r: Set<T>, p immutableP: Set<T>, x immutableX: Set<T>) -> Set<Set<T>> {
        var p = immutableP
        var x = immutableX
        if p.isEmpty, x.isEmpty {
            return [r]
        }
        let u = p.union(x).max(by: comparing { v in self.edges[v]!.count })!
        var rs: Set<Set<T>> = []
        for v in p.subtracting(edges[u]!) {
            rs.formUnion(bronKerbosch(r: r.union([v]), p: p.intersection(edges[v]!), x: x.intersection(edges[v]!)))
            p.remove(v)
            x.insert(v)
        }
        return rs
    }
}

func main() {
    let nanobots = StdIn().map(parseInput)
    var graph: Graph<Cube> = Graph()
    for a in nanobots {
        for b in nanobots {
            if a.center == b.center {
                continue
            }
            if a.overlaps(with: b) {
                graph.addEdge(between: a, and: b)
            }
        }
    }
    let cliques = graph.maximalCliques()
    let bestSize = cliques.map { clique in clique.count }.max()
    let bestCliques = cliques.filter { clique in clique.count == bestSize }
    if bestCliques.count != 1 {
        fatalError("Got \(bestCliques.count) cliques.")
    }
    let clique = bestCliques.first!
    let bestDistance = clique.map { bot in bot.center.distance(from: me) - bot.radius }.max()!
    print(bestDistance)
}

func parseInput(input: String) -> Cube {
    guard let match = inputParser.firstMatch(in: input) else {
        fatalError("Could not parse \"\(input)\".")
    }
    return Cube(
        center: Position(
            x: Int(match[1])!,
            y: Int(match[2])!,
            z: Int(match[3])!
        ),
        radius: Int(match[4])!
    )
}
