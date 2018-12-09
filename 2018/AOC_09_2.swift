let parser = try! RegExp(pattern: "^(\\d+) players; last marble is worth (\\d+) points$")

class List<T> {
    let value: T
    var before: List<T>!
    var after: List<T>!

    static func of(value: T) -> List<T> {
        let list = List<T>(value: value)
        list.before = list
        list.after = list
        return list
    }

    private init(value: T) {
        self.value = value
        before = nil
        after = nil
    }

    private init(value: T, before: List<T>, after: List<T>) {
        self.value = value
        self.before = before
        self.after = after
    }

    func traverse(backwards amount: Int) -> List<T> {
        var current = self
        for _ in 0 ..< amount {
            current = current.before
        }
        return current
    }

    func traverse(forwards amount: Int) -> List<T> {
        var current = self
        for _ in 0 ..< amount {
            current = current.after
        }
        return current
    }

    func insert(value: T) -> List<T> {
        let inserted = List(value: value, before: before, after: self)
        before.after = inserted
        before = inserted
        return inserted
    }

    func remove() -> (T, List<T>) {
        before.after = after
        after.before = before
        return (value, after)
    }

    func elements() -> [T] {
        var result = [value]
        var current: List<T> = after
        while current !== self {
            result.append(current.value)
            current = current.after
        }
        return result
    }
}

func main() {
    let match = parser.firstMatch(in: readLine()!)!
    let playerCount = Int(match[1])!
    let lastMarbleScore = Int(match[2])! * 100

    var scores = Array(repeating: 0, count: playerCount)
    var circle = List.of(value: 0)
    var currentPlayer = 0
    for i in 1 ... lastMarbleScore {
        if i % 23 == 0 {
            circle = circle.traverse(backwards: 7)
            let removed: Int
            (removed, circle) = circle.remove()
            scores[currentPlayer] += i + removed
        } else {
            circle = circle.traverse(forwards: 2)
            circle = circle.insert(value: i)
        }
        currentPlayer = (currentPlayer + 1) % playerCount
    }

    let highScore = scores.max()!
    print(highScore)
}
