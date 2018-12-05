func main() {
    let result = StdIn().map({ Int($0)! }).reduce(0, { $0 + $1 })
    print(result)
}
