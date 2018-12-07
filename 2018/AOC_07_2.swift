let minimumTime = CommandLine.arguments.count == 3 ? Int(CommandLine.arguments[1])! : 60
let workerCount = CommandLine.arguments.count == 3 ? Int(CommandLine.arguments[2])! : 5

let orderingParser = try! RegExp(pattern: "^Step (\\w) must be finished before step (\\w) can begin\\.$")

typealias Step = String
typealias Time = Int

let allSteps: [Step] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".map({ Step($0) })

extension Step {
    func timeTaken() -> Time {
        return minimumTime + allSteps.firstIndex(where: { $0 == self })! + 1
    }
}

struct Ordering {
    let before: Step
    let after: Step
}

struct Worker {
    var done: [(Step, Time)] = []

    var busyUntil: Time {
        return done.last?.1 ?? 0
    }

    mutating func handle(step: Step, at start: Time) {
        done.append((step, start + step.timeTaken()))
    }
}

func main() {
    let orderings = StdIn().map(parseOrdering)
    let orderingLookup: [Step: Set<Step>] =
        Dictionary(grouping: orderings, by: { $0.after })
        .mapValues({ steps in Set(steps.map({ $0.before })) })

    let befores: Set<Step> = Set(orderings.map({ $0.before }))
    let afters: Set<Step> = Set(orderings.map({ $0.after }))
    let steps = befores.union(afters)

    var workers = Array(repeating: Worker(), count: workerCount)

    var seen: Set<Step> = Set()
    var unseen = steps
    while !unseen.isEmpty {
        for currentTime in Set(workers.map({ $0.busyUntil })).sorted() {
            let done = Set(workers.flatMap({ worker in worker.done.filter({ _, time in time <= currentTime }).map({ step, _ in step }) }))
            let next = unseen.subtracting(done).filter({ step in (orderingLookup[step] ?? Set()).isSubset(of: done) })
            if !next.isEmpty {
                for selected in next.sorted() {
                    for i in 0 ..< workers.count {
                        if workers[i].busyUntil <= currentTime {
                            workers[i].handle(step: selected, at: currentTime)
                            seen.insert(selected)
                            unseen.remove(selected)
                            break
                        }
                    }
                }
                break
            }
        }
    }

    let timeTaken = workers.map({ $0.busyUntil }).max()!
    print(timeTaken)
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
