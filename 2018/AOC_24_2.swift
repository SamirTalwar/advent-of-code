let inputParser = try! RegExp(pattern: "^(\\d+) units each with (\\d+) hit points(?: \\((.+?)\\))? with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)$")
let weaknessParser = try! RegExp(pattern: "^weak to (.+)$")
let immunityParser = try! RegExp(pattern: "^immune to (.+)$")

enum ArmyType: String, CustomStringConvertible {
    case immuneSystem = "Immune system"
    case infection = "Infection"

    var description: String {
        return rawValue
    }
}

struct Army: Equatable {
    typealias Index = Array<Group>.Index

    let type: ArmyType
    var groups: [Group]

    var hasUnits: Bool {
        return units > 0
    }

    var units: Int {
        return groups.map({ $0.units }).reduce(0, +)
    }

    func target(_ other: Army) -> [(ArmyType, Index, ArmyType, Index)] {
        let attackingGroups = Array(
            groups
                .enumerated()
                .filter({ $0.element.isAlive })
                .sorted(by: comparing({ $0.element.initiative }))
                .sorted(by: comparing({ $0.element.effectivePower }))
                .reversed()
        )
        var defendingGroups = Array(
            other.groups.enumerated()
                .filter({ $0.element.isAlive })
                .sorted(by: comparing({ $0.element.initiative }))
                .sorted(by: comparing({ $0.element.effectivePower }))
                .reversed()
        )
        var targets: [(ArmyType, Index, ArmyType, Index)] = []
        for (attackingGroupIndex, attackingGroup) in attackingGroups {
            let damageToBeDealt = defendingGroups
                .map({ index, defendingGroup in (index, defendingGroup, attackingGroup.damage(to: defendingGroup)) })
                .filter({ _, _, damage in damage > 0 })
            guard let (defendingGroupIndex, _, _) = damageToBeDealt.max(by: comparing({ _, _, damage in damage })) else {
                continue
            }
            targets.append((type, attackingGroupIndex, other.type, defendingGroupIndex))
            defendingGroups.removeAll(where: { $0.offset == defendingGroupIndex })
        }
        return targets
    }
}

struct Group: Equatable {
    var units: Int
    let hitPoints: Int
    let modifiers: Modifiers
    let attackDamage: Int
    let attackType: AttackType
    let initiative: Int

    var isAlive: Bool {
        return units > 0
    }

    var isDead: Bool {
        return !isAlive
    }

    var effectivePower: Int {
        return units * attackDamage
    }

    func with(boost: Int) -> Group {
        return Group(
            units: units,
            hitPoints: hitPoints,
            modifiers: modifiers,
            attackDamage: attackDamage + boost,
            attackType: attackType,
            initiative: initiative
        )
    }

    func damage(to other: Group) -> Int {
        let multiplier = other.modifiers.multiplier(attackType: attackType)
        return effectivePower * multiplier
    }

    func attack(_ other: inout Group) {
        let unitsLost = damage(to: other) / other.hitPoints
        other.units -= unitsLost
        if other.units < 0 {
            other.units = 0
        }
    }
}

enum AttackType: String, Hashable, CustomStringConvertible {
    case bludgeoning
    case cold
    case fire
    case slashing
    case radiation

    var description: String {
        return rawValue
    }
}

struct Modifiers: Equatable {
    let modifiers: [AttackType: Modifier]

    init() {
        modifiers = [:]
    }

    init(_ uniqueKeysWithValues: [(AttackType, Modifier)]) {
        modifiers = Dictionary(uniqueKeysWithValues: uniqueKeysWithValues)
    }

    func multiplier(attackType: AttackType) -> Int {
        return (modifiers[attackType] ?? .normal).multiplier
    }
}

enum Modifier: String, CustomStringConvertible {
    case normal
    case immune
    case weak

    var description: String {
        return rawValue
    }

    var multiplier: Int {
        switch self {
        case .normal:
            return 1
        case .immune:
            return 0
        case .weak:
            return 2
        }
    }
}

func main() {
    let input = StdIn().split(whereSeparator: { $0.isEmpty })
    let armies: [ArmyType: Army] = [
        .immuneSystem: Army(type: .immuneSystem, groups: input[0].dropFirst().map(parseInput)),
        .infection: Army(type: .infection, groups: input[1].dropFirst().map(parseInput)),
    ]
    var start = 0
    var end = 1_000_000
    var boost = end / 2
    while start < end {
        let winner = fight(armies: armies, boost: [.immuneSystem: boost])
        if winner == nil || winner!.type == .infection {
            start = boost + 1
        } else {
            end = boost
        }
        boost = (start + end) / 2
    }
    let winner = fight(armies: armies, boost: [.immuneSystem: boost])
    print(winner?.units ?? 0)
}

func fight(armies startingArmies: [ArmyType: Army], boost: [ArmyType: Int]) -> Army? {
    var armies = startingArmies.mapValues({ army in
        Army(type: army.type, groups: army.groups.map({ group in group.with(boost: boost[army.type] ?? 0) }))
    })
    while armies.values.allSatisfy({ $0.hasUnits }) {
        let before = armies
        let targets =
            (armies[.immuneSystem]!.target(armies[.infection]!) + armies[.infection]!.target(armies[.immuneSystem]!))
            .sorted(by: comparing({ armyType, armyIndex, _, _ in armies[armyType]!.groups[armyIndex].initiative }))
            .reversed()
        for (attackerType, attackerIndex, defenderType, defenderIndex) in targets {
            let attacker = armies[attackerType]!.groups[attackerIndex]
            if attacker.isDead {
                continue
            }
            attacker.attack(&armies[defenderType]!.groups[defenderIndex])
        }
        if armies == before {
            return nil
        }
    }
    return armies.values.first(where: { $0.hasUnits })
}

func parseInput(string: String) -> Group {
    guard let match = inputParser.firstMatch(in: string) else {
        fatalError("Could not parse: \"\(string)\"")
    }
    guard let units = Int(match[1]) else {
        fatalError("Could not parse units: \(match[1])")
    }
    guard let hitPoints = Int(match[2]) else {
        fatalError("Could not parse hit points: \(match[2])")
    }
    let modifierMatch = match.matched(at: 3)
    let modifiers = modifierMatch == nil ? Modifiers() : parseModifiers(string: String(modifierMatch!))
    guard let attackDamage = Int(match[4]) else {
        fatalError("Could not parse attack damage: \(match[4])")
    }
    guard let attackType = AttackType(rawValue: String(match[5])) else {
        fatalError("Could not parse attack type: \(match[5])")
    }
    guard let initiative = Int(match[6]) else {
        fatalError("Could not parse initiative: \(match[6])")
    }
    return Group(
        units: units,
        hitPoints: hitPoints,
        modifiers: modifiers,
        attackDamage: attackDamage,
        attackType: attackType,
        initiative: initiative
    )
}

func parseModifiers(string: String) -> Modifiers {
    let modifiers = string.components(separatedBy: "; ").flatMap({ part -> [(AttackType, Modifier)] in
        if let match = weaknessParser.firstMatch(in: part) {
            return parseAttackTypes(string: String(match[1])).map({ ($0, Modifier.weak) })
        }
        if let match = immunityParser.firstMatch(in: part) {
            return parseAttackTypes(string: String(match[1])).map({ ($0, Modifier.immune) })
        }
        fatalError("Could not parse the modifier part \"\(part)\".")
    })
    return Modifiers(modifiers)
}

func parseAttackTypes(string: String) -> [AttackType] {
    return string.components(separatedBy: ", ").map({ component in
        guard let value = AttackType(rawValue: component) else {
            fatalError("Could not parse attack type: \(component)")
        }
        return value
    })
}
