let instructionPointerAssignmentParser = try! RegExp(pattern: "^#ip (\\d+)$")
let instructionParser = try! RegExp(pattern: "^(\\w+) (\\d+) (\\d+) (\\d+)$")

func main() {
    let instructionPointerAddress = parseInstructionPointerAssignment(readLine()!)
    let instructions = StdIn().map(parseInstruction)

    var registers = Registers(values: [0, 0, 0, 0, 0, 0])
    var instructionPointer = registers[instructionPointerAddress]
    while instructions.indices.contains(instructionPointer) {
        registers[instructionPointerAddress] = instructionPointer
        registers = instructions[instructionPointer].apply(to: registers)
        instructionPointer = registers[instructionPointerAddress]
        instructionPointer += 1
    }
    print(registers)
}

func parseInstructionPointerAssignment(_ string: String) -> Registers.Address {
    guard let match = instructionPointerAssignmentParser.firstMatch(in: string) else {
        fatalError("Could not parse the instruction pointer assignment:\n\"\(string)\"")
    }
    return Registers.Address(match[1])!
}

func parseInstruction(_ string: String) -> Instruction {
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
