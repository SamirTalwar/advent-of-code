typealias Recipe = UInt8

let startingRecipes: [Recipe] = [3, 7]

struct State: CustomStringConvertible {
    let recipes: [Recipe]
    let elfPositions: [Int]

    var description: String {
        return recipes.enumerated().map { recipe in
            if elfPositions[0] == recipe.offset {
                return "(\(recipe.element))"
            } else if elfPositions[1] == recipe.offset {
                return "[\(recipe.element)]"
            } else {
                return " \(recipe.element) "
            }
        }.joined()
    }

    func createNewRecipes() -> State {
        let newRecipes = recipes + digits(elfPositions.map { position in recipes[position] }.reduce(0, +))
        let newElfPositions = elfPositions.map { position in
            (position + 1 + Int(newRecipes[position])) % newRecipes.count
        }
        return State(recipes: newRecipes, elfPositions: newElfPositions)
    }
}

func main() {
    let discard = Int(readLine()!)!
    var state = State(recipes: startingRecipes, elfPositions: [0, 1])
    while state.recipes.count < discard + 10 {
        state = state.createNewRecipes()
    }

    print(state.recipes[discard ..< discard + 10].map { recipe in recipe.description }.joined())
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
