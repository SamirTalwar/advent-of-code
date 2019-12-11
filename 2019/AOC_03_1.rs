use std::collections::HashSet;
use std::convert::TryFrom;
use std::io;
use std::io::Read;

use nom::{
    character::complete as character, combinator::map, combinator::map_opt, combinator::opt,
    multi::many1, sequence::pair, sequence::terminated, sequence::tuple, IResult,
};

mod errors;
mod parse;

type WirePath = Vec<Movement>;

#[derive(Clone, Copy, Debug)]
struct Movement {
    direction: Direction,
    amount: Amount,
}

#[derive(Clone, Copy, Debug)]
enum Direction {
    Down,
    Left,
    Right,
    Up,
}

impl Direction {
    fn inc(&self, position: Coordinates) -> Coordinates {
        match self {
            Direction::Down => Coordinates {
                y: position.y - 1,
                ..position
            },
            Direction::Left => Coordinates {
                x: position.x - 1,
                ..position
            },
            Direction::Right => Coordinates {
                x: position.x + 1,
                ..position
            },
            Direction::Up => Coordinates {
                y: position.y + 1,
                ..position
            },
        }
    }
}

type Amount = u32;

#[derive(Clone, Copy, Debug, Hash)]
struct Coordinates {
    x: i32,
    y: i32,
}

impl Coordinates {
    const ZERO: Coordinates = Coordinates { x: 0, y: 0 };

    fn distance_from(&self, other: &Self) -> u32 {
        u32::try_from(i32::abs(self.x - other.x) + i32::abs(self.y - other.y)).unwrap()
    }
}

impl PartialEq for Coordinates {
    fn eq(&self, other: &Self) -> bool {
        self.x == other.x && self.y == other.y
    }
}
impl Eq for Coordinates {}

fn main() -> io::Result<()> {
    let mut input: String = String::new();
    io::stdin().read_to_string(&mut input)?;
    let (_, (wire_a, wire_b)) = parse_wire_paths(&input).map_err(errors::debug_to_io)?;

    let wire_a_locations = all_locations(&wire_a);
    let wire_b_locations = all_locations(&wire_b);
    let overlaps = wire_a_locations.intersection(&wire_b_locations);
    let distance = overlaps
        .map(|c| c.distance_from(&Coordinates::ZERO))
        .fold(std::u32::MAX, |a, b| a.min(b));
    println!("{}", distance);

    Ok(())
}

fn all_locations(wire_path: &WirePath) -> HashSet<Coordinates> {
    let mut locations = HashSet::new();
    let mut position = Coordinates::ZERO;
    for movement in wire_path {
        for _ in 0..movement.amount {
            position = movement.direction.inc(position);
            locations.insert(position);
        }
    }
    locations
}

fn parse_wire_paths(input: &str) -> IResult<&str, (WirePath, WirePath)> {
    tuple((parse_wire_path, parse_wire_path))(input)
}

fn parse_wire_path(input: &str) -> IResult<&str, WirePath> {
    terminated(
        many1(terminated(parse_movement, opt(character::char(',')))),
        opt(character::line_ending),
    )(input)
}

fn parse_movement(input: &str) -> IResult<&str, Movement> {
    map(
        pair(parse_direction, parse::digits),
        |(direction, amount)| Movement { direction, amount },
    )(input)
}

fn parse_direction(input: &str) -> IResult<&str, Direction> {
    map_opt(character::anychar, |c| match c {
        'D' => Some(Direction::Down),
        'L' => Some(Direction::Left),
        'R' => Some(Direction::Right),
        'U' => Some(Direction::Up),
        _ => None,
    })(input)
}
