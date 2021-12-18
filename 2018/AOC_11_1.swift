typealias SerialNumber = Int

typealias PowerLevel = Int

struct Cell: Hashable {
    let x: Int
    let y: Int

    static func + (cell: Cell, offset: (Int, Int)) -> Cell {
        return Cell(x: cell.x + offset.0, y: cell.y + offset.1)
    }
}

func main() {
    let serialNumber = SerialNumber(readLine()!)!

    let cells = (1 ... 300).flatMap { y in (1 ... 300).map { x in Cell(x: x, y: y) } }
    let powerLevels = Dictionary(
        uniqueKeysWithValues: cells.map { cell in
            (cell, powerLevel(of: cell, serialNumber: serialNumber))
        }
    )

    let corners = (1 ... 298).flatMap { y in (1 ... 298).map { x in Cell(x: x, y: y) } }
    let squarePairs = corners.map { corner -> (Cell, PowerLevel) in
        let squareCells = (0 ..< 3).flatMap { y in (0 ..< 3).map { x in corner + (x, y) } }
        let totalPowerLevel = squareCells.map { cell in powerLevels[cell]! }.reduce(0, +)
        return (corner, totalPowerLevel)
    }
    let squares = Dictionary(uniqueKeysWithValues: squarePairs)

    let (key: cell, value: totalPowerLevel) = squares.max(by: comparing { $0.value })!
    print(cell, "@", totalPowerLevel)
}

func powerLevel(of cell: Cell, serialNumber: SerialNumber) -> PowerLevel {
    let rackId = cell.x + 10
    var powerLevel = rackId * cell.y
    powerLevel += serialNumber
    powerLevel *= rackId
    let hundredsDigit = (powerLevel / 100) % 10
    return hundredsDigit - 5
}
