func main() {
    let ids = Array(StdIn())
    let distances = ids.enumerated()
        .flatMap { a in ids.dropFirst(a.offset)
            .map { b in distanceBetween(a.element, b) }
        }
    guard let (lettersInCommon, _) = distances.first(where: { _, distance in distance == 1 }) else {
        fatalError("Could not find two IDs with a distance of 1.")
    }
    print(lettersInCommon)
}

func distanceBetween(_ a: String, _ b: String) -> (String, Int) {
    if a.count != b.count {
        fatalError("The two strings are not the same length")
    }
    let lettersInCommon = String(zip(a, b).filter { x, y in x == y }.map { x, _ in x })
    return (lettersInCommon, a.count - lettersInCommon.count)
}
