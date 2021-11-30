enum ElfCode {
    typealias Input = Int

    typealias Output = Registers.Address

    struct Registers: Equatable {
        typealias Address = Int
        typealias Value = Int

        var values: [Value]

        subscript(index: Address) -> Value {
            get {
                return values[index]
            }
            set(value) {
                values[index] = value
            }
        }

        func with(value newValue: Value, at index: Address) -> Registers {
            var newValues = values
            newValues[index] = newValue
            return Registers(values: newValues)
        }

        func with(comparison: Bool, at index: Address) -> Registers {
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

        static func value(of string: String) -> Operation {
            switch string {
            case "addr":
                return .addr
            case "addi":
                return .addi
            case "mulr":
                return .mulr
            case "muli":
                return .muli
            case "banr":
                return .banr
            case "bani":
                return .bani
            case "borr":
                return .borr
            case "bori":
                return .bori
            case "setr":
                return .setr
            case "seti":
                return .seti
            case "gtir":
                return .gtir
            case "gtri":
                return .gtri
            case "gtrr":
                return .gtrr
            case "eqir":
                return .eqir
            case "eqri":
                return .eqri
            case "eqrr":
                return .eqrr
            default:
                fatalError("Could not process the operation \"\(string)\".")
            }
        }
    }

    struct Instruction {
        let operation: Operation
        let inputA: Input
        let inputB: Input
        let output: Output

        func apply(to registers: Registers) -> Registers {
            switch operation {
            case .addr:
                return registers.with(value: registers[inputA] + registers[inputB], at: output)
            case .addi:
                return registers.with(value: registers[inputA] + inputB, at: output)
            case .mulr:
                return registers.with(value: registers[inputA] * registers[inputB], at: output)
            case .muli:
                return registers.with(value: registers[inputA] * inputB, at: output)
            case .banr:
                return registers.with(value: registers[inputA] & registers[inputB], at: output)
            case .bani:
                return registers.with(value: registers[inputA] & inputB, at: output)
            case .borr:
                return registers.with(value: registers[inputA] | registers[inputB], at: output)
            case .bori:
                return registers.with(value: registers[inputA] | inputB, at: output)
            case .setr:
                return registers.with(value: registers[inputA], at: output)
            case .seti:
                return registers.with(value: inputA, at: output)
            case .gtir:
                return registers.with(comparison: inputA > registers[inputB], at: output)
            case .gtri:
                return registers.with(comparison: registers[inputA] > inputB, at: output)
            case .gtrr:
                return registers.with(comparison: registers[inputA] > registers[inputB], at: output)
            case .eqir:
                return registers.with(comparison: inputA == registers[inputB], at: output)
            case .eqri:
                return registers.with(comparison: registers[inputA] == inputB, at: output)
            case .eqrr:
                return registers.with(comparison: registers[inputA] == registers[inputB], at: output)
            }
        }
    }

    static let instructionPointerAssignmentParser = try! RegExp(pattern: "^#ip (\\d+)$")
    static let instructionParser = try! RegExp(pattern: "^(\\w+) (\\d+) (\\d+) (\\d+)$")

    static func parseInstructionPointerAssignment(_ string: String) -> Registers.Address {
        guard let match = instructionPointerAssignmentParser.firstMatch(in: string) else {
            fatalError("Could not parse the instruction pointer assignment:\n\"\(string)\"")
        }
        return Registers.Address(match[1])!
    }

    static func parseInstruction(_ string: String) -> Instruction {
        guard let match = instructionParser.firstMatch(in: string) else {
            fatalError("Could not parse the instruction:\n\"\(string)\"")
        }
        return Instruction(
            operation: Operation.value(of: String(match[1])),
            inputA: Input(match[2])!,
            inputB: Input(match[3])!,
            output: Output(match[4])!
        )
    }
}
