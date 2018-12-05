func comparing<T, U: Comparable>(_ transform: @escaping (T) -> U) -> (T, T) -> Bool {
    return { a, b in transform(a) < transform(b) }
}
