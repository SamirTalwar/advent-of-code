use std::fmt::Debug;
use std::io;
use std::ops::RangeFrom;
use std::str::FromStr;

use nom::{
    character::complete as character, combinator::all_consuming, combinator::map_res,
    combinator::opt, sequence::pair, IResult,
};

use super::errors;

pub fn completely<I, O, F>(parse: F) -> impl FnOnce(I) -> io::Result<O>
where
    I: Debug + nom::InputLength,
    F: Fn(I) -> IResult<I, O>,
{
    |input| {
        all_consuming(parse)(input)
            .map(|(_, parsed)| parsed)
            .map_err(errors::debug_to_io)
    }
}

pub fn digits<I, O>(input: I) -> IResult<I, O>
where
    I: Clone
        + Into<String>
        + nom::InputIter
        + nom::InputTakeAtPosition
        + nom::Slice<RangeFrom<usize>>,
    <I as nom::InputIter>::Item: nom::AsChar,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar,
    O: FromStr,
{
    map_res(
        pair(opt(character::char('-')), character::digit1),
        |(sign, digits): (Option<char>, I)| {
            (sign.map(|s| s.to_string()).unwrap_or("".to_string()) + &digits.into()).parse::<O>()
        },
    )(input)
}
