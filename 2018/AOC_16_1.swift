import Foundation

let beforeAfterParser = try! RegExp(pattern: "^\\w+: +\\[([0-9, ]+)\\]$")

typealias OpCode = Int

typealias UnknownInstruction = (OpCode, ElfCode.Input, ElfCode.Input, ElfCode.Output)

struct Sample {
    let instruction: UnknownInstruction
    let before: ElfCode.Registers
    let after: ElfCode.Registers
}

func main() {
    let samples = parseSamples(lines: Array(StdIn()))
    var count = 0
    for sample in samples {
        let validInstructionCount =
            ElfCode.Operation.allCases
                .filter { operation in
                    let instruction = ElfCode.Instruction(
                        operation: operation,
                        inputA: sample.instruction.1,
                        inputB: sample.instruction.2,
                        output: sample.instruction.3
                    )
                    let actual = instruction.apply(to: sample.before)
                    return actual == sample.after
                }
                .count
        if validInstructionCount >= 3 {
            count += 1
        }
    }
    print(count)
}

func parseSamples(lines: [String]) -> [Sample] {
    let chunks: [[String]] = Array(
        lines
            .split(separator: "", maxSplits: Int.max, omittingEmptySubsequences: false)
            .prefix(while: { value in !value.isEmpty })
            .map { chunk in Array(chunk) }
    )
    return chunks.map(parseSample)
}

func parseSample(chunk: [String]) -> Sample {
    let instructionValues = chunk[1].split(separator: " ").map { value in Int(value)! }
    let instruction = (
        instructionValues[0],
        instructionValues[1],
        instructionValues[2],
        instructionValues[3]
    )
    return Sample(
        instruction: instruction,
        before: parseBeforeAfter(chunk[0]),
        after: parseBeforeAfter(chunk[2])
    )
}

func parseBeforeAfter(_ string: String) -> ElfCode.Registers {
    guard let match = beforeAfterParser.firstMatch(in: string) else {
        fatalError("Could not parse \"\(string)\".")
    }
    return ElfCode.Registers(
        values: match[1]
            .split(separator: ",")
            .map { value in Int(value.trimmingCharacters(in: CharacterSet.whitespaces))! }
    )
}
