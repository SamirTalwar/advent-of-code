import Foundation

struct RegExp {
    private let expression: NSRegularExpression

    init(pattern: String) throws {
        expression = try NSRegularExpression(pattern: pattern)
    }

    func firstMatch(in string: String) -> RegExpMatch? {
        guard let match = expression.firstMatch(in: string, range: NSMakeRange(0, string.count)) else {
            return nil
        }
        return RegExpMatch(string: string, match: match)
    }
}

struct RegExpMatch {
    let string: String
    let match: NSTextCheckingResult

    var count: Int {
        return match.numberOfRanges
    }

    subscript(index: Int) -> Substring {
        return string[Range(match.range(at: index), in: string)!]
    }
}
