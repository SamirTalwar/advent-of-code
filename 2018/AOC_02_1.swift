func main() {
    let ids = Array(StdIn())
    let result =
        countWithLettersAppearingExactly(input: ids, times: 2)
        * countWithLettersAppearingExactly(input: ids, times: 3)
    print(result)
}

func countWithLettersAppearingExactly(input: [String], times: Int) -> Int {
    return input.filter({ $0.charactersAppearingExactly(times: times).count > 0 }).count
}

extension String {
    func charactersAppearingExactly(times: Int) -> Set<Character> {
        return Set(
            Dictionary(grouping: self, by: { $0 })
                .mapValues({ $0.count })
                .filter({ $0.1 == times })
                .keys
        )
    }
}
