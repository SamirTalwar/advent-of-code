use std::cmp;

use num;

pub type Digit = u8;

pub type Digits = Vec<Digit>;

pub fn to_digits<N>(number: N) -> Digits
where
    N: num::Num + num::NumCast + num::ToPrimitive + cmp::PartialOrd + Copy,
{
    let zero: N = num::zero();
    let ten: N = N::from(10).unwrap();

    if number == zero {
        return vec![0];
    }
    let mut result = Vec::new();
    let mut current = number;
    while current > zero {
        result.push((current % ten).to_u8().unwrap());
        current = current / ten;
    }
    result.reverse();
    result
}

pub fn from_digits<N>(digits: Digits) -> N
where
    N: num::Num + num::NumCast + Copy,
{
    let ten = N::from(10).unwrap();
    digits
        .iter()
        .rfold(
            (num::zero(), num::one()),
            |(accumulator, multiplier): (N, N), digit| {
                (
                    accumulator + N::from(*digit).unwrap() * multiplier,
                    multiplier * ten,
                )
            },
        )
        .0
        .into()
}
