import Foundation

let beforeAfterParser = try! RegExp(pattern: "^\\w+: +\\[([0-9, ]+)\\]$")

typealias OpCode = Int

typealias RegisterAddress = Int

typealias Input = Int

typealias Output = RegisterAddress

typealias UnknownInstruction = (OpCode, Input, Input, Output)

struct Registers: Equatable {
    typealias Value = Int

    let values: [Int]

    subscript(index: RegisterAddress) -> Value {
        return values[index]
    }

    func with(value newValue: Value, at index: RegisterAddress) -> Registers {
        var newValues = values
        newValues[index] = newValue
        return Registers(values: newValues)
    }

    func with(comparison: Bool, at index: RegisterAddress) -> Registers {
        var newValues = values
        newValues[index] = comparison ? 1 : 0
        return Registers(values: newValues)
    }
}

enum Instruction {
    case addr(registerA: Input, registerB: Input, output: Output)
    case addi(register: Input, value: Input, output: Output)
    case mulr(registerA: Input, registerB: Input, output: Output)
    case muli(register: Input, value: Input, output: Output)
    case banr(registerA: Input, registerB: Input, output: Output)
    case bani(register: Input, value: Input, output: Output)
    case borr(registerA: Input, registerB: Input, output: Output)
    case bori(register: Input, value: Input, output: Output)
    case setr(register: Input, ignored: Input, output: Output)
    case seti(value: Input, ignored: Input, output: Output)
    case gtir(value: Input, register: Input, output: Output)
    case gtri(register: Input, value: Input, output: Output)
    case gtrr(registerA: Input, registerB: Input, output: Output)
    case eqir(value: Input, register: Input, output: Output)
    case eqri(register: Input, value: Input, output: Output)
    case eqrr(registerA: Input, registerB: Input, output: Output)

    static var allCases: [(_ a: Input, _ b: Input, _ c: Output) -> Instruction] {
        return [
            Instruction.addr, Instruction.addi,
            Instruction.mulr, Instruction.muli,
            Instruction.banr, Instruction.bani,
            Instruction.borr, Instruction.bori,
            Instruction.setr, Instruction.seti,
            Instruction.gtir, Instruction.gtri, Instruction.gtrr,
            Instruction.eqir, Instruction.eqri, Instruction.eqrr,
        ]
    }

    func apply(to registers: Registers) -> Registers {
        switch self {
        case let .addr(a, b, output):
            return registers.with(value: registers[a] + registers[b], at: output)
        case let .addi(a, b, output):
            return registers.with(value: registers[a] + b, at: output)
        case let .mulr(a, b, output):
            return registers.with(value: registers[a] * registers[b], at: output)
        case let .muli(a, b, output):
            return registers.with(value: registers[a] * b, at: output)
        case let .banr(a, b, output):
            return registers.with(value: registers[a] & registers[b], at: output)
        case let .bani(a, b, output):
            return registers.with(value: registers[a] & b, at: output)
        case let .borr(a, b, output):
            return registers.with(value: registers[a] | registers[b], at: output)
        case let .bori(a, b, output):
            return registers.with(value: registers[a] | b, at: output)
        case let .setr(a, _, output):
            return registers.with(value: registers[a], at: output)
        case let .seti(a, _, output):
            return registers.with(value: a, at: output)
        case let .gtir(a, b, output):
            return registers.with(comparison: a > registers[b], at: output)
        case let .gtri(a, b, output):
            return registers.with(comparison: registers[a] > b, at: output)
        case let .gtrr(a, b, output):
            return registers.with(comparison: registers[a] > registers[b], at: output)
        case let .eqir(a, b, output):
            return registers.with(comparison: a == registers[b], at: output)
        case let .eqri(a, b, output):
            return registers.with(comparison: registers[a] == b, at: output)
        case let .eqrr(a, b, output):
            return registers.with(comparison: registers[a] == registers[b], at: output)
        }
    }
}

struct Sample {
    let instruction: UnknownInstruction
    let before: Registers
    let after: Registers
}

func main() {
    let samples = parseSamples(lines: Array(StdIn()))
    var count = 0
    for sample in samples {
        let validInstructionCount =
            Instruction.allCases
            .map({ newInstruction in newInstruction(sample.instruction.1, sample.instruction.2, sample.instruction.3) })
            .filter({ instruction in instruction.apply(to: sample.before) == sample.after })
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
            .map({ chunk in Array(chunk) })
    )
    return chunks.map(parseSample)
}

func parseSample(chunk: [String]) -> Sample {
    let instructionValues = chunk[1].split(separator: " ").map({ value in Int(value)! })
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

func parseBeforeAfter(_ string: String) -> Registers {
    guard let match = beforeAfterParser.firstMatch(in: string) else {
        fatalError("Could not parse \"\(string)\".")
    }
    return Registers(
        values: match[1]
            .split(separator: ",")
            .map({ value in Int(value.trimmingCharacters(in: CharacterSet.whitespaces))! })
    )
}
