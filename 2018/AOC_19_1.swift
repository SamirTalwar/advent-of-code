func main() {
    let instructionPointerAddress = ElfCode.parseInstructionPointerAssignment(readLine()!)
    let instructions = StdIn().map(ElfCode.parseInstruction)

    var registers = ElfCode.Registers(values: [0, 0, 0, 0, 0, 0])
    var instructionPointer = registers[instructionPointerAddress]
    while instructions.indices.contains(instructionPointer) {
        registers[instructionPointerAddress] = instructionPointer
        registers = instructions[instructionPointer].apply(to: registers)
        instructionPointer = registers[instructionPointerAddress]
        instructionPointer += 1
    }
    print(registers)
}
