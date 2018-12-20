func comparing<T, U: Comparable>(_ transform: @escaping (T) -> U) -> (T, T) -> Bool {
    return { a, b in transform(a) < transform(b) }
}

extension Array: Comparable where Element: Comparable {
    public static func < (lhs: Array<Element>, rhs: Array<Element>) -> Bool {
        for i in 0 ..< Swift.min(lhs.count, rhs.count) {
            if lhs[i] != rhs[i] {
                return lhs[i] < rhs[i]
            }
        }
        return lhs.count < rhs.count
    }
}
