func main() {
    let numbers = StdIn().map { Int($0)! }
    var seen: Set<Int> = []
    var i = 0
    var current = 0
    while !seen.contains(current) {
        seen.insert(current)
        current += numbers[i]
        i = (i + 1) % numbers.count
    }
    print(current)
}
