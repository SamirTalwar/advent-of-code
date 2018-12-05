import Foundation

class StdIn: Sequence, IteratorProtocol {
    typealias Element = String

    func next() -> Element? {
        return readLine()
    }
}
