let iterations: Int64 = 50_000_000_000

typealias Offset = Int64

enum Pot {
    case empty
    case withPlant
}

extension Pot: CustomStringConvertible {
    var description: String {
        switch self {
        case .empty:
            return "."
        case .withPlant:
            return "#"
        }
    }
}

struct Pots: Hashable, BidirectionalCollection, CustomStringConvertible {
    typealias Element = Pot
    typealias Collection = [Element]
    typealias Index = Collection.Index
    typealias Iterator = Collection.Iterator

    private let pots: Collection

    var description: String {
        return pots.map { pot in pot.description }.joined()
    }

    var count: Int {
        return pots.count
    }

    var startIndex: Index {
        return pots.startIndex
    }

    var endIndex: Index {
        return pots.endIndex
    }

    init(_ pots: Collection) {
        self.pots = pots
    }

    subscript(index: Index) -> Element {
        if index < startIndex || index >= endIndex {
            return Pot.empty
        }
        return pots[index]
    }

    func index(before index: Index) -> Index {
        return pots.index(before: index)
    }

    func index(after index: Index) -> Index {
        return pots.index(after: index)
    }

    func index(index: Index, offsetBy distance: Int) -> Index {
        return pots.index(index, offsetBy: distance)
    }

    func makeIterator() -> Iterator {
        return pots.makeIterator()
    }

    func shrink() -> (Pots, Offset) {
        guard let dropFromStart = pots.firstIndex(where: { pot in pot == Pot.withPlant }) else {
            return (Pots([]), 0)
        }
        let dropFromEnd = pots.lastIndex(where: { pot in pot == Pot.withPlant })!
        let shrunkPots = Array(pots[dropFromStart ... dropFromEnd])
        return (Pots(shrunkPots), Offset(dropFromStart))
    }
}

typealias Spreading = [Pots: Pot]

struct State: Hashable, CustomStringConvertible {
    let pots: Pots
    let offset: Offset

    var description: String {
        return pots.description
    }

    var result: Offset {
        return pots.enumerated()
            .reduce(0) { value, pot in value + (pot.element == Pot.withPlant ? Offset(pot.offset) + offset : 0) }
    }

    init(pots: Pots) {
        self.init(pots: pots, offset: 0)
    }

    init(pots: Pots, offset: Offset) {
        let (shrunkPots, shrinkingOffset) = pots.shrink()
        self.pots = shrunkPots
        self.offset = offset + shrinkingOffset
    }

    func spread(spreading: Spreading) -> State {
        let indices = pots.index(pots.startIndex, offsetBy: -2) ... pots.index(pots.endIndex, offsetBy: 2)
        let newPots = indices.map { i -> Pot in
            let surrounding = Pots([self.pots[i - 2], self.pots[i - 1], self.pots[i], self.pots[i + 1], self.pots[i + 2]])
            return spreading[surrounding] ?? Pot.empty
        }
        return State(pots: Pots(newPots), offset: offset - 2)
    }
}

let initialStateParser = try! RegExp(pattern: "^initial state: ([\\.#]+)$")
let spreadingParser = try! RegExp(pattern: "^([\\.#]+) => ([\\.#])$")

func main() {
    let initialState = parseInitialState(in: readLine()!)
    _ = readLine()!
    let spreading = Dictionary(uniqueKeysWithValues: StdIn().map(parseSpreading))
    var state = initialState
    for i in 1 ... iterations {
        let nextState = state.spread(spreading: spreading)
        if state.pots == nextState.pots {
            state = State(pots: nextState.pots, offset: nextState.offset + (nextState.offset - state.offset) * (iterations - i))
            break
        }
        state = nextState
    }
    print(state.result)
}

func parseInitialState(in string: String) -> State {
    let match = initialStateParser.firstMatch(in: string)!
    return parseState(in: String(match[1]))
}

func parseSpreading(in string: String) -> (Pots, Pot) {
    let match = spreadingParser.firstMatch(in: string)!
    return (parsePots(in: String(match[1])), parsePot(in: match[2].first!))
}

func parseState(in string: String) -> State {
    return State(pots: parsePots(in: string))
}

func parsePots(in string: String) -> Pots {
    return Pots(string.map(parsePot))
}

func parsePot(in character: Character) -> Pot {
    return character == "#" ? Pot.withPlant : Pot.empty
}
