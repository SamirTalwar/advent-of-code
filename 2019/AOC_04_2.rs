use std::collections::HashSet;
use std::io;

use nom;

mod digits;
mod parse;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let (_, (start, end)) =
        parse(&input).map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{:?}", err)))?;

    let valid: HashSet<u32> = (start..end + 1)
        .map(digits::to_digits)
        .filter(exactly_two_adjacent_digits_are_the_same)
        .filter(digits_never_decrease)
        .map(digits::from_digits)
        .collect();

    println!("{}", valid.len());

    Ok(())
}

fn exactly_two_adjacent_digits_are_the_same(digits: &digits::Digits) -> bool {
    let mut group_counts: Vec<u32> = Vec::new();
    let mut last_digit: Option<digits::Digit> = None;
    for digit in digits {
        if last_digit != Some(*digit) {
            group_counts.push(0);
        }
        let last_index = group_counts.len() - 1;
        group_counts[last_index] += 1;
        last_digit = Some(*digit);
    }
    group_counts.iter().any(|count| *count == 2)
}

fn digits_never_decrease(digits: &digits::Digits) -> bool {
    digits.windows(2).all(|window| window[1] >= window[0])
}

fn parse(input: &str) -> nom::IResult<&str, (u32, u32)> {
    nom::sequence::tuple((
        nom::sequence::terminated(parse::digits, nom::character::complete::char('-')),
        parse::digits,
    ))(input)
}
