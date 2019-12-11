use std::io;

mod digits;
mod errors;

const IMAGE_WIDTH: usize = 25;
const IMAGE_HEIGHT: usize = 6;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let image_size = IMAGE_WIDTH * IMAGE_HEIGHT;

    let digits: digits::Digits = input
        .trim()
        .chars()
        .map(|c| {
            c.to_digit(10)
                .map(|digit| digit as digits::Digit)
                .ok_or(errors::io(&format!("Invalid digit: {}", c)))
        })
        .collect::<io::Result<_>>()?;

    let layer = digits
        .chunks(image_size)
        .min_by_key(|layer| layer.iter().filter(|pixel| **pixel == 0).count())
        .ok_or(errors::io("No layers!"))?;
    let ones = layer.iter().filter(|pixel| **pixel == 1).count();
    let twos = layer.iter().filter(|pixel| **pixel == 2).count();
    println!("{} * {} = {}", ones, twos, ones * twos);

    Ok(())
}
