let gridSize = 300

typealias SerialNumber = Int

typealias PowerLevel = Int

func main() {
    let serialNumber = SerialNumber(readLine()!)!

    var summedArea = Array(repeating: Array(repeating: 0, count: gridSize + 1), count: gridSize + 1)
    for y in 1 ... gridSize {
        for x in 1 ... gridSize {
            summedArea[y][x] =
                powerLevel(x: x, y: y, serialNumber: serialNumber)
                    + summedArea[y][x - 1]
                    + summedArea[y - 1][x]
                    - summedArea[y - 1][x - 1]
        }
    }

    var currentLargest: (Int, Int, Int, PowerLevel) = (0, 0, 0, 0)
    for y in 1 ... gridSize {
        for x in 1 ... gridSize {
            for size in 1 ... (gridSize - max(x, y) + 1) {
                let total = summedArea[y - 1][x - 1]
                    - summedArea[y - 1][x - 1 + size]
                    - summedArea[y - 1 + size][x - 1]
                    + summedArea[y - 1 + size][x - 1 + size]
                if total > currentLargest.3 {
                    currentLargest = (x, y, size, total)
                }
            }
        }
    }

    print(currentLargest)
}

func powerLevel(x: Int, y: Int, serialNumber: SerialNumber) -> PowerLevel {
    let rackId = x + 10
    var powerLevel = rackId * y
    powerLevel += serialNumber
    powerLevel *= rackId
    let hundredsDigit = (powerLevel / 100) % 10
    return hundredsDigit - 5
}
