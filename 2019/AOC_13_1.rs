use std::io;

mod digits;
mod errors;
mod intcode;

#[derive(Debug, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl Tile {
    fn from_code(code: intcode::Code) -> Self {
        match code {
            0 => Self::Empty,
            1 => Self::Wall,
            2 => Self::Block,
            3 => Self::Paddle,
            4 => Self::Ball,
            _ => panic!("Invalid tile code: {}", code),
        }
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let program = intcode::parse(&input)?;
    let device = intcode::VecDevice::new(Vec::new());
    let (_, outputs) = intcode::evaluate(program, device);

    let block_count = outputs
        .chunks(3)
        .map(|chunk| chunk[2])
        .map(Tile::from_code)
        .filter(|tile| *tile == Tile::Block)
        .count();
    println!("{}", block_count);

    Ok(())
}
