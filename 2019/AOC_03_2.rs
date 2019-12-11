use std::collections::HashMap;
use std::collections::HashSet;
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

type Distance = u32;

#[derive(Clone, Copy, Debug, Hash)]
struct Coordinates {
    x: i32,
    y: i32,
}

impl Coordinates {
    const ZERO: Coordinates = Coordinates { x: 0, y: 0 };
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

    let wire_a_locations_and_distances = all_locations_and_distances(&wire_a);
    let wire_b_locations_and_distances = all_locations_and_distances(&wire_b);
    let wire_a_locations = wire_a_locations_and_distances
        .keys()
        .collect::<HashSet<&Coordinates>>();
    let wire_b_locations = wire_b_locations_and_distances
        .keys()
        .collect::<HashSet<&Coordinates>>();
    let overlapping_locations = wire_a_locations.intersection(&wire_b_locations);
    let best = overlapping_locations
        .map(|c| wire_a_locations_and_distances[c] + wire_b_locations_and_distances[c])
        .min()
        .ok_or(io::ErrorKind::NotFound)?;
    println!("{}", best);

    Ok(())
}

fn all_locations_and_distances(wire_path: &WirePath) -> HashMap<Coordinates, Distance> {
    let mut locations = HashMap::new();
    let mut position = Coordinates::ZERO;
    let mut steps = 0;
    for movement in wire_path {
        for _ in 0..movement.amount {
            steps += 1;
            position = movement.direction.inc(position);
            if !locations.contains_key(&position) {
                locations.insert(position, steps);
            }
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
