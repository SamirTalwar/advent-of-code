import Foundation

let beforeAfterParser = try! RegExp(pattern: "^\\w+: +\\[([0-9, ]+)\\]$")

typealias OpCode = Int

let allOpCodes: [OpCode] = Array(0 ..< 16)

typealias RegisterAddress = Int

typealias Input = Int

typealias Output = RegisterAddress

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

enum Operation: CaseIterable, CustomStringConvertible {
    case addr
    case addi
    case mulr
    case muli
    case banr
    case bani
    case borr
    case bori
    case setr
    case seti
    case gtir
    case gtri
    case gtrr
    case eqir
    case eqri
    case eqrr

    var description: String {
        switch self {
        case .addr:
            return "addr"
        case .addi:
            return "addi"
        case .mulr:
            return "mulr"
        case .muli:
            return "muli"
        case .banr:
            return "banr"
        case .bani:
            return "bani"
        case .borr:
            return "borr"
        case .bori:
            return "bori"
        case .setr:
            return "setr"
        case .seti:
            return "seti"
        case .gtir:
            return "gtir"
        case .gtri:
            return "gtri"
        case .gtrr:
            return "gtrr"
        case .eqir:
            return "eqir"
        case .eqri:
            return "eqri"
        case .eqrr:
            return "eqrr"
        }
    }

    func apply(a: Input, b: Input, output: Output, to registers: Registers) -> Registers {
        switch self {
        case .addr:
            return registers.with(value: registers[a] + registers[b], at: output)
        case .addi:
            return registers.with(value: registers[a] + b, at: output)
        case .mulr:
            return registers.with(value: registers[a] * registers[b], at: output)
        case .muli:
            return registers.with(value: registers[a] * b, at: output)
        case .banr:
            return registers.with(value: registers[a] & registers[b], at: output)
        case .bani:
            return registers.with(value: registers[a] & b, at: output)
        case .borr:
            return registers.with(value: registers[a] | registers[b], at: output)
        case .bori:
            return registers.with(value: registers[a] | b, at: output)
        case .setr:
            return registers.with(value: registers[a], at: output)
        case .seti:
            return registers.with(value: a, at: output)
        case .gtir:
            return registers.with(comparison: a > registers[b], at: output)
        case .gtri:
            return registers.with(comparison: registers[a] > b, at: output)
        case .gtrr:
            return registers.with(comparison: registers[a] > registers[b], at: output)
        case .eqir:
            return registers.with(comparison: a == registers[b], at: output)
        case .eqri:
            return registers.with(comparison: registers[a] == b, at: output)
        case .eqrr:
            return registers.with(comparison: registers[a] == registers[b], at: output)
        }
    }
}

typealias UnknownInstruction = (OpCode, Input, Input, Output)

typealias Instruction = (Operation, Input, Input, Output)

struct Sample {
    let instruction: UnknownInstruction
    let before: Registers
    let after: Registers
}

func main() {
    let (samples, program) = parse(lines: Array(StdIn()))
    let opCodeMappings = figureOutOpCodes(from: samples)
    let instructions = program.map({ opCode, a, b, output in (opCodeMappings[opCode]!, a, b, output) })
    let registers = execute(instructions: instructions, registers: Registers(values: [0, 0, 0, 0]))
    print("Registers:", registers.values)
}

func execute(instructions: [Instruction], registers: Registers) -> Registers {
    var currentRegisters = registers
    for instruction in instructions {
        currentRegisters = instruction.0.apply(
            a: instruction.1,
            b: instruction.2,
            output: instruction.3,
            to: currentRegisters
        )
    }
    return currentRegisters
}

func figureOutOpCodes(from samples: [Sample]) -> [OpCode: Operation] {
    let allOpCodes: Set<OpCode> = Set(samples.map({ sample in sample.instruction.0 }))
    let allOperations: Set<Operation> = Set(Operation.allCases)
    var validOperations: [OpCode: Set<Operation>] = Dictionary(
        uniqueKeysWithValues: allOpCodes.map({ opCode in (opCode, allOperations) })
    )

    for sample in samples {
        let sampleValidOperations =
            Operation.allCases
            .filter({ operation in
                let actual = operation.apply(
                    a: sample.instruction.1,
                    b: sample.instruction.2,
                    output: sample.instruction.3,
                    to: sample.before
                )
                return actual == sample.after
            })
        validOperations[sample.instruction.0]!.formIntersection(sampleValidOperations)
    }

    while let (opCodeToSkip, operationToRemove) = findUniqueOperation(in: validOperations) {
        for opCode in validOperations.keys {
            if opCode != opCodeToSkip {
                validOperations[opCode]!.remove(operationToRemove)
            }
        }
    }

    let opCodeMappings = validOperations.mapValues({ operations -> Operation in
        if operations.count != 1 {
            fatalError("Multiple operations for an opcode: \(operations)")
        }
        return operations.first!
    })
    if opCodeMappings.keys.count != 16 {
        fatalError("There aren't enough opcode mappings.")
    }
    return opCodeMappings
}

func findUniqueOperation(in validOperations: [OpCode: Set<Operation>]) -> (OpCode, Operation)? {
    for (opCode, operations) in validOperations {
        if operations.count == 1 {
            let operation = operations.first!
            let otherOperations = validOperations.filter({ otherOpCode, otherOperations in
                opCode != otherOpCode && otherOperations.contains(operation)
            })
            if otherOperations.isEmpty {
                continue
            }
            return (opCode, operation)
        }
    }
    return nil
}

func parse(lines: [String]) -> ([Sample], [UnknownInstruction]) {
    let chunks =
        lines
        .split(separator: "")
        .map({ chunk in Array(chunk) })
    let sampleSection = chunks.dropLast()
    let programSection = chunks.last!
    let samples: [Sample] = Array(sampleSection.map(parseSample))
    let instructions: [UnknownInstruction] = Array(programSection.map(parseInstruction))
    return (samples, instructions)
}

func parseSample(chunk: [String]) -> Sample {
    return Sample(
        instruction: parseInstruction(chunk[1]),
        before: parseBeforeAfter(chunk[0]),
        after: parseBeforeAfter(chunk[2])
    )
}

func parseInstruction(_ string: String) -> UnknownInstruction {
    let values = string.split(separator: " ").map({ value in Int(value)! })
    return (values[0], values[1], values[2], values[3])
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
