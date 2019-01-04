func main() {
    let instructionPointerAddress = ElfCode.parseInstructionPointerAssignment(readLine()!)
    let instructions = StdIn().map(ElfCode.parseInstruction)

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

func primeFactors(number: Int) -> [Int] {
    var factors: [Int] = []
    for i in 1 ... number + 1 {
        if number % i == 0 {
            factors.append(i)
        }
    }
    return factors
}
