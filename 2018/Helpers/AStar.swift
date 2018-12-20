typealias Cost = Int

struct AStar<T> where T: Hashable, T: Comparable {
    typealias Route = [T]

    let neighbors: (T) -> [(T, Cost)]
    let costEstimate: (T, T) -> Cost

    init(neighbors: @escaping (T) -> [T], costEstimate: @escaping (T, T) -> Cost) {
        self.neighbors = { node in neighbors(node).map({ neighbor in (neighbor, 1) }) }
        self.costEstimate = costEstimate
    }

    init(neighbors: @escaping (T) -> [(T, Cost)], costEstimate: @escaping (T, T) -> Cost) {
        self.neighbors = neighbors
        self.costEstimate = costEstimate
    }

    func shortestPath(from start: T, to end: T) -> Route? {
        var closedSet: Set<T> = []
        var openSet = [start]
        var cameFrom: [T: T] = [:]
        var costFromStart: [T: Cost] = [start: 0]
        var estimatedCostToEnd: [T: Cost] = [start: costEstimate(start, end)]

        while let current = openSet.min(by: comparing({ node in estimatedCostToEnd[node] ?? Cost.max })) {
            let routeToCurrent = lazily({ self.reconstructPath(to: current, cameFrom: cameFrom) })
            if current == end {
                return routeToCurrent.value
            }

            openSet.removeAll(where: { value in value == current })
            closedSet.insert(current)

            for (neighbor, distance) in neighbors(current) {
                if closedSet.contains(neighbor) {
                    continue
                }

                let tentativeCostFromStart = costFromStart[current]! + distance

                if !openSet.contains(neighbor) {
                    openSet.append(neighbor)
                }

                if let neighborCost = costFromStart[neighbor] {
                    if tentativeCostFromStart > neighborCost {
                        continue
                    }
                    if tentativeCostFromStart == neighborCost {
                        let existingRoute = reconstructPath(to: neighbor, cameFrom: cameFrom)
                        let proposedRoute = routeToCurrent.value + [neighbor]
                        if existingRoute <= proposedRoute {
                            continue
                        }
                    }
                }

                cameFrom[neighbor] = current
                costFromStart[neighbor] = tentativeCostFromStart
                estimatedCostToEnd[neighbor] = tentativeCostFromStart + costEstimate(neighbor, end)
            }
        }

        return nil
    }

    private func reconstructPath(to end: T, cameFrom: [T: T]) -> Route {
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
