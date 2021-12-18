typealias Recipe = UInt8

let startingRecipes: [Recipe] = [3, 7]

struct State: CustomStringConvertible {
    var recipes: [Recipe]
    var elfAPosition: Int
    var elfBPosition: Int

    var description: String {
        return recipes.enumerated().map { recipe in
            if elfAPosition == recipe.offset {
                return "(\(recipe.element))"
            } else if elfBPosition == recipe.offset {
                return "[\(recipe.element)]"
            } else {
                return " \(recipe.element) "
            }
        }.joined()
    }

    mutating func createNewRecipes() {
        recipes += digits(recipes[elfAPosition] + recipes[elfBPosition])
        elfAPosition = (elfAPosition + 1 + Int(recipes[elfAPosition])) % recipes.count
        elfBPosition = (elfBPosition + 1 + Int(recipes[elfBPosition])) % recipes.count
    }
}

func main() {
    let marker: [Recipe] = readLine()!.map { character in Recipe(String(character))! }
    var state = State(recipes: startingRecipes, elfAPosition: 0, elfBPosition: 1)
    var searchResult = SearchResult.notFound(stoppingAtIndex: 0)
    while true {
        switch searchResult {
        case let .found(atIndex: index):
            print(index)
            return
        case let .notFound(stoppingAtIndex: lastIndexChecked):
            state.createNewRecipes()
            searchResult = search(array: state.recipes, forValues: marker, startingAt: lastIndexChecked)
        }
    }
}

func digits(_ number: Recipe) -> [Recipe] {
    if number < 10 {
        return [number]
    } else if number < 20 {
        return [1, number % 10]
    } else {
        fatalError("Did not expect to compute the digits of \(number).")
    }
}

enum SearchResult {
    case notFound(stoppingAtIndex: Int)
    case found(atIndex: Int)
}

func search<T>(array: [T], forValues subarray: [T], startingAt startIndex: Int) -> SearchResult where T: Equatable {
    let endIndex = array.count - subarray.count
    if array.count < subarray.count || startIndex > endIndex {
        return .notFound(stoppingAtIndex: startIndex)
    }

    if subarray.count == 0 {
        return .found(atIndex: startIndex)
    }

    for i in startIndex ... endIndex {
        if array[i] == subarray[0] {
            var successful = true
            for j in 1 ..< subarray.count {
                if array[i + j] != subarray[j] {
                    successful = false
                    break
                }
            }
            if successful {
                return .found(atIndex: i)
            }
        }
    }

    return .notFound(stoppingAtIndex: endIndex)
}
