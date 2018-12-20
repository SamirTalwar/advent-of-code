func lazily<T>(_ constructor: @escaping () -> T) -> Lazy<T> {
    return Lazy(constructor)
}

class Lazy<T> {
    let constructor: () -> T
    var computedValue: T?

    init(_ constructor: @escaping () -> T) {
        self.constructor = constructor
    }

    var value: T {
        if let computedValue = computedValue {
            return computedValue
        }
        computedValue = constructor()
        return computedValue!
    }
}
