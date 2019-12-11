use std::collections::HashSet;
use std::io;

use nom;

mod digits;
mod errors;
mod parse;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    let (_, (start, end)) = parse(&input).map_err(errors::debug_to_io)?;

    let valid: HashSet<u32> = (start..end + 1)
        .map(digits::to_digits)
        .filter(two_adjacent_digits_are_the_same)
        .filter(digits_never_decrease)
        .map(digits::from_digits)
        .collect();

    println!("{}", valid.len());

    Ok(())
}

fn two_adjacent_digits_are_the_same(digits: &digits::Digits) -> bool {
    digits.windows(2).any(|window| window[0] == window[1])
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
