use std::str::FromStr;

use nom::{character::complete as character, combinator::map_res, IResult};

pub fn digits<T>(input: &str) -> IResult<&str, T>
where
    T: FromStr,
{
    map_res(character::digit1, |digits: &str| digits.parse::<T>())(input)
}
