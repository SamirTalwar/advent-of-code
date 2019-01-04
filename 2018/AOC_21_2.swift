func main() {
    var seen: [ElfCode.Registers.Value] = []
    var b = 0
    while !seen.contains(b) {
        seen.append(b)
        b = next(b: b)
    }
    print(seen.last!)
}

func next(b previousB: ElfCode.Registers.Value) -> ElfCode.Registers.Value {
    var e = previousB | 0x10000
    var b = 16_298_264
    while true {
        b += e & 0xFF
        b &= 0xFFFFFF
        b *= 65899
        b &= 0xFFFFFF
        if e < 256 {
            break
        }
        e /= 256
    }
    return b
}

//  0 seti 123 _ b
//  1 bani b 456 b
//  2 eqri b 72 b
//  3 addr b ip ip
//  4 seti 0 _ ip
//  5 seti 0 _ b
//  6 bori b 65536 e
//  7 seti 16298264 _ b
//  8 bani e 255 f
//  9 addr b f b
// 10 bani b 16777215 b
// 11 muli b 65899 b
// 12 bani b 16777215 b
// 13 gtir 256 e f
// 14 addr f ip ip
// 15 addi ip 1 ip
// 16 seti 27 _ ip
// 17 seti 0 _ f
// 18 addi f 1 d
// 19 muli d 256 d
// 20 gtrr d e d
// 21 addr d ip ip
// 22 addi ip 1 ip
// 23 seti 25 _ ip
// 24 addi f 1 f
// 25 seti 17 _ ip
// 26 setr f _ e
// 27 seti 7 _ ip
// 28 eqrr b a f
// 29 addr f ip ip
// 30 seti 5 _ ip
