let gridSize = 300

typealias SerialNumber = Int

typealias PowerLevel = Int

struct Cell: Hashable {
    let x: Int
    let y: Int

    static func + (cell: Cell, offset: (Int, Int)) -> Cell {
        return Cell(x: cell.x + offset.0, y: cell.y + offset.1)
    }
}

struct Square: Hashable {
    let corner: Cell
    let size: Int

    var bottomAndRightCells: [Cell] {
        let max = size - 1
        let bottomRightCorner = corner + (max, max)
        let bottom = (0 ..< max).map({ x in self.corner + (x, max) })
        let right = (0 ..< max).map({ y in self.corner + (max, y) })
        return [bottomRightCorner] + bottom + right
    }
}

func main() {
    let serialNumber = SerialNumber(readLine()!)!

    let cells = (1 ... gridSize).flatMap({ y in (1 ... gridSize).map({ x in Cell(x: x, y: y) }) })
    let powerLevels = Dictionary(
        uniqueKeysWithValues: cells.map({ cell in
            (cell, powerLevel(of: cell, serialNumber: serialNumber))
        })
    )

    var currentLargest: (Square, PowerLevel) = (Square(corner: Cell(x: 0, y: 0), size: 0), 0)
    for cell in cells {
        var powerLevel = 0
        for size in 1 ... (gridSize - max(cell.x, cell.y) + 1) {
            let square = Square(corner: cell, size: size)
            powerLevel += square.bottomAndRightCells.map({ cell in powerLevels[cell]! }).reduce(0, +)
            if powerLevel > currentLargest.1 {
                currentLargest = (square, powerLevel)
            }
        }
    }

    print(currentLargest)
}

func powerLevel(of cell: Cell, serialNumber: SerialNumber) -> PowerLevel {
    let rackId = cell.x + 10
    var powerLevel = rackId * cell.y
    powerLevel += serialNumber
    powerLevel *= rackId
    let hundredsDigit = (powerLevel / 100) % 10
    return hundredsDigit - 5
}
