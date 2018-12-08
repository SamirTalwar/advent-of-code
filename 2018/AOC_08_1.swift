typealias Data = Int

typealias Metadata = Int

struct Node {
    let metadata: [Metadata]
    let children: [Node]

    var allMetadata: [Metadata] {
        return metadata + children.flatMap { child in child.allMetadata }
    }
}

func main() {
    let tokens = readLine()!.split(separator: " ").map({ Data($0)! })
    let (tree, _) = parseNode(data: tokens.dropFirst(0))
    let sum = tree.allMetadata.reduce(0, +)
    print(sum)
}

func parseNode(data: ArraySlice<Data>) -> (Node, ArraySlice<Data>) {
    let childrenCount = data[data.startIndex]
    let metadataCount = data[data.index(after: data.startIndex)]
    var children: [Node] = []
    var remaining = data.dropFirst(2)
    for _ in 0 ..< childrenCount {
        let child: Node
        (child, remaining) = parseNode(data: remaining)
        children.append(child)
    }
    let metadata = Array(remaining.prefix(metadataCount))
    remaining = remaining.dropFirst(metadataCount)
    return (Node(metadata: metadata, children: children), remaining)
}
