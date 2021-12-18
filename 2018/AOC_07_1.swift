let orderingParser = try! RegExp(pattern: "^Step (\\w) must be finished before step (\\w) can begin\\.$")

typealias Step = String

struct Ordering {
    let before: Step
    let after: Step
}

func main() {
    let orderings = StdIn().map(parseOrdering)
    let orderingLookup: [Step: Set<Step>] =
        Dictionary(grouping: orderings, by: { $0.after })
            .mapValues { steps in Set(steps.map { $0.before }) }

    let befores: Set<Step> = Set(orderings.map { $0.before })
    let afters: Set<Step> = Set(orderings.map { $0.after })
    let steps = befores.union(afters)

    var sequence: [Step] = []
    var seen: Set<Step> = Set()
    var unseen = steps
    while true {
        let next = unseen.filter { step in (orderingLookup[step] ?? Set()).isSubset(of: seen) }.subtracting(seen)
        guard let selected = next.sorted().first else {
            break
        }
        sequence.append(selected)
        seen.insert(selected)
        unseen.remove(selected)
    }
    print(sequence.joined())
}

func parseOrdering(string: String) -> Ordering {
    guard let match = orderingParser.firstMatch(in: string) else {
        fatalError("This ordering was unparseable: \(string)")
    }
    return Ordering(
        before: String(match[1]),
        after: String(match[2])
    )
}
