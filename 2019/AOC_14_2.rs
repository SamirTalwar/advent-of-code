use std::cmp::Reverse;
use std::collections::HashMap;
use std::hash::Hash;
use std::io;
use std::io::BufRead;
use std::ops::Mul;

use nom;

mod errors;
mod parse;

const ORE: &'static str = "ORE";

const FUEL: &'static str = "FUEL";

const ORE_AMOUNT: usize = 1_000_000_000_000;

type Magnitude = usize;

type Chemical = String;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Amount {
    magnitude: Magnitude,
    chemical: Chemical,
}

impl Mul<Magnitude> for Amount {
    type Output = Amount;

    fn mul(self, multiplier: Magnitude) -> Self::Output {
        Amount {
            magnitude: self.magnitude * multiplier,
            ..self
        }
    }
}

type Reaction = (Vec<Amount>, Amount);

type InverseReactions = HashMap<Chemical, (Magnitude, Vec<Amount>)>;

type Depth = usize;

type Depths = HashMap<Chemical, Depth>;

type Needed = Vec<Amount>;

fn main() -> io::Result<()> {
    let mut inverse_reactions = io::stdin()
        .lock()
        .lines()
        .map(|line_result| {
            line_result
                .and_then(|line| parse::completely(parse_reaction)(line.as_str()))
                .map(|(from, to)| (to.chemical, (to.magnitude, from)))
        })
        .collect::<io::Result<InverseReactions>>()?;

    let mut chemical_depths: Depths = [(ORE.to_string(), 0)].into_iter().cloned().collect();
    inverse_reactions.keys().for_each(|chemical| {
        count_depth(&chemical, &inverse_reactions, &mut chemical_depths);
    });
    inverse_reactions
        .iter_mut()
        .for_each(|(_, (_, required_amounts))| {
            required_amounts.sort_by_key(|amount| Reverse(chemical_depths[&amount.chemical]));
        });

    let fuel = binary_search_range_max(1, ORE_AMOUNT, |fuel_amount| {
        let needed: Needed = vec![Amount {
            chemical: FUEL.to_string(),
            magnitude: fuel_amount,
        }];
        let ore = calculate_ore_required(needed, &inverse_reactions, &chemical_depths);
        ore <= ORE_AMOUNT
    });

    println!("{}", fuel.unwrap());

    Ok(())
}

fn count_depth(
    chemical: &Chemical,
    inverse_reactions: &InverseReactions,
    depths: &mut Depths,
) -> Depth {
    if !depths.contains_key(chemical) {
        let nested_depth = inverse_reactions[chemical]
            .1
            .iter()
            .map(|amount| count_depth(&amount.chemical, inverse_reactions, depths))
            .max()
            .unwrap();
        upsert(depths, chemical.clone(), 1 + nested_depth, |a, b| a.min(b));
    }
    depths[chemical]
}

fn calculate_ore_required(
    needed: Needed,
    inverse_reactions: &InverseReactions,
    depths: &Depths,
) -> Magnitude {
    if needed.len() == 1 && needed[0].chemical.as_str() == ORE {
        return needed[0].magnitude;
    }
    let needed_amount = needed
        .iter()
        .filter(|amount| amount.chemical.as_str() != ORE)
        .next()
        .unwrap();
    let (magnitude_produced, amounts_needed) = &inverse_reactions[&needed_amount.chemical];
    let mut next_hashmap: HashMap<Chemical, Magnitude> = needed
        .iter()
        .cloned()
        .map(|amount| (amount.chemical, amount.magnitude))
        .collect();
    next_hashmap.remove(&needed_amount.chemical);
    let multiple = div_rounding_up(&needed_amount.magnitude, magnitude_produced);
    amounts_needed
        .iter()
        .cloned()
        .map(move |amount_needed| amount_needed * multiple)
        .for_each(|amount_needed| {
            upsert(
                &mut next_hashmap,
                amount_needed.chemical,
                amount_needed.magnitude,
                |a, b| a + b,
            )
        });
    let mut next: Needed = next_hashmap
        .into_iter()
        .map(|(chemical, magnitude)| Amount {
            chemical,
            magnitude,
        })
        .collect();
    next.sort_by_key(|amount| Reverse(depths[&amount.chemical]));
    calculate_ore_required(next, inverse_reactions, depths)
}

fn parse_reaction(input: &str) -> nom::IResult<&str, Reaction> {
    nom::sequence::tuple((
        nom::multi::separated_nonempty_list(nom::bytes::complete::tag(", "), parse_amount),
        nom::sequence::preceded(nom::bytes::complete::tag(" => "), parse_amount),
    ))(input)
}

fn parse_amount(input: &str) -> nom::IResult<&str, Amount> {
    nom::combinator::map(
        nom::sequence::tuple((
            nom::sequence::terminated(parse::digits, nom::bytes::complete::tag(" ")),
            nom::character::complete::alpha1,
        )),
        |(magnitude, chemical): (Magnitude, &str)| Amount {
            magnitude,
            chemical: chemical.to_string(),
        },
    )(input)
}

fn div_rounding_up(numerator: &Magnitude, divisor: &Magnitude) -> Magnitude {
    let round_up = if numerator % divisor > 0 { 1 } else { 0 };
    numerator / divisor + round_up
}

fn upsert<K, V>(map: &mut HashMap<K, V>, key: K, value: V, merge: impl Fn(V, V) -> V)
where
    K: Eq + Hash,
{
    if let Some(old_value) = map.remove(&key) {
        let merged = merge(old_value, value);
        map.insert(key, merged);
    } else {
        map.insert(key, value);
    }
}

fn binary_search_range_max(
    start: usize,
    end: usize,
    predicate: impl Fn(usize) -> bool,
) -> Option<usize> {
    if start > end {
        panic!(
            "Binary search ended up searching between {} and {}.",
            start, end
        );
    } else if start == end {
        Some(start)
    } else {
        let midpoint = start + (end - start) / 2 + 1;
        if predicate(midpoint) {
            binary_search_range_max(midpoint, end, predicate)
        } else {
            binary_search_range_max(start, midpoint - 1, predicate)
        }
    }
}
