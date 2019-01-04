class BreadthFirstSearch<T> where T: Hashable {
    typealias Route = [T]

    let neighbors: (T) -> [T]

    init(neighbors: @escaping (T) -> [T]) {
        self.neighbors = neighbors
    }

    func shortestPath(from start: T, to end: T) -> Route? {
        var cameFrom: [T: T] = [:]
        var queue = [start]

        while let current = queue.first {
            if current == end {
                return reconstructRoute(to: current, cameFrom: cameFrom)
            }

            queue.remove(at: 0)
            for neighbor in neighbors(current) {
                if cameFrom[neighbor] == nil {
                    queue.append(neighbor)
                    cameFrom[neighbor] = current
                }
            }
        }

        return nil
    }

    private func reconstructRoute(to end: T, cameFrom: [T: T]) -> Route {
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
