let instructionPointerAssignmentParser = try! RegExp(pattern: "^#ip (\\d+)$")
let instructionParser = try! RegExp(pattern: "^(\\w+) (\\d+) (\\d+) (\\d+)$")

func main() {
    let instructionPointerAddress = parseInstructionPointerAssignment(readLine()!)
    let instructions = StdIn().map(parseInstruction)

    var registers = ElfCode.Registers(values: [1, 0, 0, 0, 0, 0])
    var instructionPointer = registers[instructionPointerAddress]
    while instructionPointer != 1 {
        registers[instructionPointerAddress] = instructionPointer
        registers = instructions[instructionPointer].apply(to: registers)
        instructionPointer = registers[instructionPointerAddress]
        instructionPointer += 1
    }

    let result = primeFactors(number: registers[5]).reduce(0, +)
    print(result)
}

func parseInstructionPointerAssignment(_ string: String) -> ElfCode.Registers.Address {
    guard let match = instructionPointerAssignmentParser.firstMatch(in: string) else {
        fatalError("Could not parse the instruction pointer assignment:\n\"\(string)\"")
    }
    return ElfCode.Registers.Address(match[1])!
}

func parseInstruction(_ string: String) -> ElfCode.Instruction {
    guard let match = instructionParser.firstMatch(in: string) else {
        fatalError("Could not parse the instruction:\n\"\(string)\"")
    }
    return ElfCode.Instruction(
        operation: ElfCode.Operation.value(of: String(match[1])),
        inputA: ElfCode.Input(match[2])!,
        inputB: ElfCode.Input(match[3])!,
        output: ElfCode.Output(match[4])!
    )
}

func primeFactors(number: Int) -> [Int] {
    var factors: [Int] = []
    for i in 1 ... number + 1 {
        if number % i == 0 {
            factors.append(i)
        }
    }
    return factors
}
