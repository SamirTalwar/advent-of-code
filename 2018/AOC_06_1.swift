import Foundation

typealias Region = Int

let MANY_CLOSEST: Region = -1

typealias Grid = [[Region?]]

struct Coordinates: Hashable {
    let x: Int
    let y: Int

    func present(in grid: Grid) -> Bool {
        return grid[y][x] != nil
    }
}

struct Location: Hashable {
    let region: Region
    let coordinates: Coordinates

    var x: Int {
        return coordinates.x
    }

    var y: Int {
        return coordinates.y
    }

    func offset(by bounds: Bounds) -> Location {
        return Location(
            region: region,
            coordinates: Coordinates(
                x: coordinates.x - bounds.left,
                y: coordinates.y - bounds.top
            )
        )
    }
}

struct Bounds {
    let left: Int
    let top: Int
    let right: Int
    let bottom: Int

    var width: Int {
        return right - left + 1
    }

    var height: Int {
        return bottom - top + 1
    }

    func reorient() -> Bounds {
        return Bounds(
            left: 0,
            top: 0,
            right: right - left,
            bottom: bottom - top
        )
    }

    func isInside(_ coordinates: Coordinates) -> Bool {
        return coordinates.x >= left && coordinates.x <= right && coordinates.y >= top && coordinates.y <= bottom
    }
}

func main() {
    let locations: Set<Location> = Set(StdIn().enumerated().map { line -> Location in
        let split = line.element.split(separator: ",")
        let x = Int(split[0].trimmingCharacters(in: CharacterSet.whitespaces))!
        let y = Int(split[1].trimmingCharacters(in: CharacterSet.whitespaces))!
        return Location(region: Region(line.offset), coordinates: Coordinates(x: x, y: y))
    })

    let xs: [Int] = locations.map { $0.x }
    let ys: [Int] = locations.map { $0.y }
    let bounds = Bounds(
        left: xs.min()!,
        top: ys.min()!,
        right: xs.max()!,
        bottom: ys.max()!
    )

    let gridLocations: Set<Location> = Set(locations.map { $0.offset(by: bounds) })
    let gridBounds = bounds.reorient()

    var grid: Grid = Array(repeating: Array(repeating: nil, count: gridBounds.width), count: gridBounds.height)
    for location in gridLocations {
        grid[location.y][location.x] = location.region
    }

    var distance = 1
    while propagate(grid: &grid, distance: distance, locations: gridLocations, bounds: gridBounds) {
        distance += 1
    }

    let disqualified: Set<Region> = Set([grid.first!, grid.last!, grid.map { $0.first! }, grid.map { $0.last! }].joined().filter { $0 != nil }.map { $0! })
    let qualified = Set(locations.map { $0.region }).subtracting(disqualified)

    var counts: [Region: Int] = [:]
    for y in 0 ..< gridBounds.height {
        for x in 0 ..< gridBounds.width {
            if let nearest = grid[y][x] {
                if qualified.contains(nearest) {
                    counts[nearest] = (counts[nearest] ?? 0) + 1
                }
            }
        }
    }

    let result = counts.max(by: comparing { _, count in count })!
    print("Region: \(result.key)")
    print("Size: \(result.value)")
}

func propagate(grid: inout Grid, distance: Int, locations: Set<Location>, bounds: Bounds) -> Bool {
    var coordinatesToSet: [Coordinates: Set<Region>] = [:]
    for location in locations {
        for coordinates in coordinates(of: distance, location: location, bounds: bounds) {
            if !coordinates.present(in: grid) {
                if coordinatesToSet[coordinates] == nil {
                    coordinatesToSet[coordinates] = []
                }
                coordinatesToSet[coordinates]!.insert(location.region)
            }
        }
    }

    if coordinatesToSet.isEmpty {
        return false
    }

    for (coordinates, regions) in coordinatesToSet {
        if regions.count > 1 {
            grid[coordinates.y][coordinates.x] = MANY_CLOSEST
        } else {
            grid[coordinates.y][coordinates.x] = regions.first!
        }
    }
    return true
}

func coordinates(of distance: Int, location: Location, bounds: Bounds) -> Set<Coordinates> {
    var coordinates: Set<Coordinates> = []
    for xOffset in 0 ... distance {
        let yOffset = distance - xOffset
        coordinates.insert(Coordinates(x: location.x + xOffset, y: location.y + yOffset))
        coordinates.insert(Coordinates(x: location.x - xOffset, y: location.y + yOffset))
        coordinates.insert(Coordinates(x: location.x + xOffset, y: location.y - yOffset))
        coordinates.insert(Coordinates(x: location.x - xOffset, y: location.y - yOffset))
    }
    return coordinates.filter(bounds.isInside)
}
