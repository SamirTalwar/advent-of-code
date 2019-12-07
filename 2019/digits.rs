pub type Digit = u8;

pub type Digits = Vec<Digit>;

pub fn to_digits(number: u32) -> Digits {
    if number == 0 {
        return vec![0];
    }
    let mut result = Vec::new();
    let mut current = number;
    while current > 0 {
        result.push((current % 10) as Digit);
        current /= 10;
    }
    result.reverse();
    result
}

pub fn from_digits(digits: Digits) -> u32 {
    digits
        .iter()
        .rfold((0u32, 1u32), |(accumulator, multiplier), digit| {
            (
                accumulator + u32::from(*digit) * multiplier,
                multiplier * 10,
            )
        })
        .0
        .into()
}
