typealias GuardId = Int

struct Date: Comparable, Hashable {
    let year: Int
    let month: Int
    let day: Int

    static func == (lhs: Date, rhs: Date) -> Bool {
        return lhs.year == rhs.year
            && lhs.month == rhs.month
            && lhs.day == rhs.day
    }

    static func < (lhs: Date, rhs: Date) -> Bool {
        if lhs.year != rhs.year {
            return lhs.year < rhs.year
        }
        if lhs.month != rhs.month {
            return lhs.month < rhs.month
        }
        return lhs.day < rhs.day
    }
}

struct Time: Comparable, Hashable {
    let hour: Int
    let minute: Int

    static func == (lhs: Time, rhs: Time) -> Bool {
        return lhs.hour == rhs.hour
            && lhs.minute == rhs.minute
    }

    static func < (lhs: Time, rhs: Time) -> Bool {
        if lhs.hour != rhs.hour {
            return lhs.hour < rhs.hour
        }
        return lhs.minute < rhs.minute
    }

    static func + (value: Time, amount: Int) -> Time {
        return Time(
            hour: (value.hour + (amount / 60)) % 24,
            minute: (value.minute + amount) % 60
        )
    }
}

struct Timestamp: Comparable, Hashable {
    let date: Date
    let time: Time

    static func == (lhs: Timestamp, rhs: Timestamp) -> Bool {
        return lhs.date == rhs.date
            && lhs.time == rhs.time
    }

    static func < (lhs: Timestamp, rhs: Timestamp) -> Bool {
        if lhs.date != rhs.date {
            return lhs.date < rhs.date
        }
        return lhs.time < rhs.time
    }
}

struct Record: Comparable {
    let timestamp: Timestamp
    let note: Note

    static func == (lhs: Record, rhs: Record) -> Bool {
        return lhs.timestamp == rhs.timestamp
    }

    static func < (lhs: Record, rhs: Record) -> Bool {
        return lhs.timestamp < rhs.timestamp
    }
}

enum Note {
    case beginsShift(guardId: GuardId)
    case fallsAsleep
    case wakesUp
}

func main() {
    var records = StdIn().map(parseRecord)
    records.sort()

    var asleep: [GuardId: [Time: Int]] = [:]
    var currentGuardId: GuardId!
    var timeFallenAsleep: Time?
    for record in records {
        switch record.note {
        case let .beginsShift(guardId):
            currentGuardId = guardId
        case .fallsAsleep:
            timeFallenAsleep = record.timestamp.time
        case .wakesUp:
            if asleep[currentGuardId] == nil {
                asleep[currentGuardId] = [:]
            }
            var time = timeFallenAsleep!
            while time != record.timestamp.time {
                asleep[currentGuardId]![time] = (asleep[currentGuardId]![time] ?? 0) + 1
                time = time + 1
            }
            timeFallenAsleep = nil
        }
    }

    let (idOfGuardAsleepMost, times) = asleep.max(by: comparing { _, times in times.values.reduce(0) { $0 + $1 } })!
    let (timeAsleepMost, _) = times.max(by: comparing { _, count in count })!
    let result = idOfGuardAsleepMost * timeAsleepMost.minute
    print("\(idOfGuardAsleepMost) * \(timeAsleepMost.minute) = \(result)")
}

let timestampParser = "\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\]"
let beginsShiftParser = try! RegExp(pattern: "^\(timestampParser) Guard #(\\d+) begins shift$")
let fallsAsleepParser = try! RegExp(pattern: "^\(timestampParser) falls asleep$")
let wakesUpParser = try! RegExp(pattern: "^\(timestampParser) wakes up$")

func parseRecord(string: String) -> Record {
    if let match = beginsShiftParser.firstMatch(in: string) {
        return Record(
            timestamp: extractTimestamp(match: match),
            note: Note.beginsShift(guardId: GuardId(match[6])!)
        )
    }
    if let match = fallsAsleepParser.firstMatch(in: string) {
        return Record(
            timestamp: extractTimestamp(match: match),
            note: Note.fallsAsleep
        )
    }
    if let match = wakesUpParser.firstMatch(in: string) {
        return Record(
            timestamp: extractTimestamp(match: match),
            note: Note.wakesUp
        )
    }
    fatalError("Could not parse: \(string)")
}

func extractTimestamp(match: RegExpMatch) -> Timestamp {
    return Timestamp(
        date: Date(
            year: Int(match[1])!,
            month: Int(match[2])!,
            day: Int(match[3])!
        ),
        time: Time(
            hour: Int(match[4])!,
            minute: Int(match[5])!
        )
    )
}
