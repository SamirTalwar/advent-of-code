let parser = try! RegExp(pattern: "^(\\d+) players; last marble is worth (\\d+) points$")

func main() {
    let match = parser.firstMatch(in: readLine()!)!
    let playerCount = Int(match[1])!
    let lastMarbleScore = Int(match[2])!

    var scores = Array(repeating: 0, count: playerCount)
    var circle = [0]
    var currentPlayer = 0
    var currentMarblePosition = 0
    for i in 1 ... lastMarbleScore {
        if i % 23 == 0 {
            currentMarblePosition = ((currentMarblePosition - 7 + circle.count) % circle.count)
            let removed = circle.remove(at: currentMarblePosition)
            scores[currentPlayer] += i + removed
        } else {
            currentMarblePosition = ((currentMarblePosition + 1) % circle.count) + 1
            circle.insert(i, at: currentMarblePosition)
        }
        currentPlayer = (currentPlayer + 1) % playerCount
    }

    let highScore = scores.max()!
    print(highScore)
}
