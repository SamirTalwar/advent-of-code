import Foundation

typealias Region = Int

let MANY_CLOSEST: Region = -1

let MAXIMUM_DISTANCE: Int = 10000

typealias Grid = [[Region?]]

struct Coordinates: Hashable {
    let x: Int
    let y: Int
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

    func distance(from coordinates: Coordinates) -> Int {
        return abs(x - coordinates.x) + abs(y - coordinates.y)
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
}

func main() {
    let locations: Set<Location> = Set(StdIn().enumerated().map({ line -> Location in
        let split = line.element.split(separator: ",")
        let x = Int(split[0].trimmingCharacters(in: CharacterSet.whitespaces))!
        let y = Int(split[1].trimmingCharacters(in: CharacterSet.whitespaces))!
        return Location(region: Region(line.offset), coordinates: Coordinates(x: x, y: y))
    }))

    let xs: [Int] = locations.map({ $0.x })
    let ys: [Int] = locations.map({ $0.y })
    let bounds = Bounds(
        left: xs.min()!,
        top: ys.min()!,
        right: xs.max()!,
        bottom: ys.max()!
    )

    let gridLocations: Set<Location> = Set(locations.map({ $0.offset(by: bounds) }))

    var count = 0
    for y in 0 ..< bounds.height {
        for x in 0 ..< bounds.width {
            let totalDistance = gridLocations
                .map({ location in location.distance(from: Coordinates(x: x, y: y)) })
                .reduce(0, { $0 + $1 })
            if totalDistance < MAXIMUM_DISTANCE {
                count += 1
            }
        }
    }
    print(count)
}
