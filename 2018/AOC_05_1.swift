let opposites = [
    "A".first!: "a".first!,
    "B".first!: "b".first!,
    "C".first!: "c".first!,
    "D".first!: "d".first!,
    "E".first!: "e".first!,
    "F".first!: "f".first!,
    "G".first!: "g".first!,
    "H".first!: "h".first!,
    "I".first!: "i".first!,
    "J".first!: "j".first!,
    "K".first!: "k".first!,
    "L".first!: "l".first!,
    "M".first!: "m".first!,
    "N".first!: "n".first!,
    "O".first!: "o".first!,
    "P".first!: "p".first!,
    "Q".first!: "q".first!,
    "R".first!: "r".first!,
    "S".first!: "s".first!,
    "T".first!: "t".first!,
    "U".first!: "u".first!,
    "V".first!: "v".first!,
    "W".first!: "w".first!,
    "X".first!: "x".first!,
    "Y".first!: "y".first!,
    "Z".first!: "z".first!,
    "a".first!: "A".first!,
    "b".first!: "B".first!,
    "c".first!: "C".first!,
    "d".first!: "D".first!,
    "e".first!: "E".first!,
    "f".first!: "F".first!,
    "g".first!: "G".first!,
    "h".first!: "H".first!,
    "i".first!: "I".first!,
    "j".first!: "J".first!,
    "k".first!: "K".first!,
    "l".first!: "L".first!,
    "m".first!: "M".first!,
    "n".first!: "N".first!,
    "o".first!: "O".first!,
    "p".first!: "P".first!,
    "q".first!: "Q".first!,
    "r".first!: "R".first!,
    "s".first!: "S".first!,
    "t".first!: "T".first!,
    "u".first!: "U".first!,
    "v".first!: "V".first!,
    "w".first!: "W".first!,
    "x".first!: "X".first!,
    "y".first!: "Y".first!,
    "z".first!: "Z".first!,
]

func main() {
    var polymer = readLine()!
    while react(&polymer) {}
    print(polymer.count)
}

func react(_ polymer: inout String) -> Bool {
    var successful = false
    var i = polymer.startIndex
    while i != polymer.index(before: polymer.endIndex) {
        let next = polymer.index(after: i)
        if opposites[polymer[i]]! == polymer[next] {
            polymer.removeSubrange(i ... next)
            successful = true
        } else {
            i = next
        }
    }
    return successful
}
