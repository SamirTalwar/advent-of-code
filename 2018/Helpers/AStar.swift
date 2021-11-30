typealias Cost = Int

struct AStar<T> where T: Hashable {
    typealias Route = [T]

    struct Path {
        let route: Route
        let cost: Cost
    }

    let neighbors: (T) -> [(T, Cost)]
    let costEstimate: (T, T) -> Cost

    init(neighbors: @escaping (T) -> [T], costEstimate: @escaping (T, T) -> Cost) {
        self.neighbors = { node in neighbors(node).map { neighbor in (neighbor, 1) } }
        self.costEstimate = costEstimate
    }

    init(neighbors: @escaping (T) -> [(T, Cost)], costEstimate: @escaping (T, T) -> Cost) {
        self.neighbors = neighbors
        self.costEstimate = costEstimate
    }

    func shortestPath(from start: T, to end: T) -> Path? {
        var closedSet: Set<T> = []
        var openSet: Set<T> = [start]
        var cameFrom: [T: T] = [:]
        var costFromStart: [T: Cost] = [start: 0]
        var estimatedCostToEnd: [T: Cost] = [start: costEstimate(start, end)]

        while let current = openSet.min(by: comparing { node in estimatedCostToEnd[node] ?? Cost.max }) {
            if current == end {
                let route = AStar.reconstructRoute(to: current, cameFrom: cameFrom)
                let cost = costFromStart[current]!
                return Path(route: route, cost: cost)
            }

            openSet.remove(current)
            closedSet.insert(current)

            for (neighbor, distance) in neighbors(current) {
                if closedSet.contains(neighbor) {
                    continue
                }

                let tentativeCostFromStart = costFromStart[current]! + distance

                if !openSet.contains(neighbor) {
                    openSet.insert(neighbor)
                } else if let neighborCost = costFromStart[neighbor] {
                    if tentativeCostFromStart >= neighborCost {
                        continue
                    }
                }

                cameFrom[neighbor] = current
                costFromStart[neighbor] = tentativeCostFromStart
                estimatedCostToEnd[neighbor] = tentativeCostFromStart + costEstimate(neighbor, end)
            }
        }

        return nil
    }

    private static func reconstructRoute(to end: T, cameFrom: [T: T]) -> Route {
        var current = end
        var totalPath = [current]
        while let previous = cameFrom[current] {
            current = previous
            totalPath.append(current)
        }
        totalPath.reverse()
        totalPath.remove(at: 0)
        return totalPath
    }
}
