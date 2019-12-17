use std::cmp::Ordering;
use std::io;
use std::io::BufRead;
use std::iter;
use std::ops::Add;

use nom;
use nom::bytes::complete as bytes;
use nom::character::complete as character;

mod errors;
mod parse;

type Energy = usize;

#[derive(Debug, Clone, Copy)]
struct Coordinates {
    x: isize,
    y: isize,
    z: isize,
}

impl Coordinates {
    const ZERO: Self = Self { x: 0, y: 0, z: 0 };

    fn energy(&self) -> Energy {
        self.x.abs() as usize + self.y.abs() as usize + self.z.abs() as usize
    }
}

impl Add for Coordinates {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Coordinates {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Moon {
    position: Coordinates,
    velocity: Coordinates,
}

impl Moon {
    fn apply_gravity(self, moons: &Moons) -> Self {
        let change = moons
            .iter()
            .map(|other| Coordinates {
                x: Self::pull(self.position.x, other.position.x),
                y: Self::pull(self.position.y, other.position.y),
                z: Self::pull(self.position.z, other.position.z),
            })
            .fold(Coordinates::ZERO, |a, b| a + b);
        Self {
            velocity: self.velocity + change,
            ..self
        }
    }

    fn apply_velocity(&self) -> Self {
        Self {
            position: self.position + self.velocity,
            ..*self
        }
    }

    fn energy(&self) -> Energy {
        self.potential_energy() * self.kinetic_energy()
    }

    fn potential_energy(&self) -> Energy {
        self.position.energy()
    }

    fn kinetic_energy(&self) -> Energy {
        self.velocity.energy()
    }

    fn pull(a: isize, b: isize) -> isize {
        match a.cmp(&b) {
            Ordering::Less => 1,
            Ordering::Equal => 0,
            Ordering::Greater => -1,
        }
    }
}

#[derive(Debug, Clone)]
struct Moons(Vec<Moon>);

impl iter::FromIterator<Moon> for Moons {
    fn from_iter<T: IntoIterator<Item = Moon>>(iterator: T) -> Self {
        Moons(iterator.into_iter().collect())
    }
}

impl Moons {
    fn iter(&self) -> impl Iterator<Item = &Moon> {
        self.0.iter()
    }

    fn step(&self) -> Self {
        self.iter()
            .cloned()
            .map(move |moon| moon.apply_gravity(&self))
            .map(|moon| moon.apply_velocity())
            .collect()
    }

    fn energy(&self) -> Energy {
        self.iter().map(|moon| moon.energy()).sum()
    }
}

fn main() -> io::Result<()> {
    let lines: Vec<String> = io::BufReader::new(io::stdin())
        .lines()
        .collect::<io::Result<_>>()?;
    let moons: Moons = lines
        .iter()
        .map(|line| parse::completely(parse)(line))
        .collect::<io::Result<_>>()?;

    let steps = iter::successors(Some(moons), |moons| Some(moons.step()));
    let last_step = steps.take(1001).last().unwrap();

    println!("{}", last_step.energy());

    Ok(())
}

fn parse(input: &str) -> nom::IResult<&str, Moon> {
    nom::combinator::map(
        nom::sequence::tuple((
            nom::sequence::preceded(
                character::char('<'),
                nom::sequence::terminated(
                    nom::sequence::preceded(bytes::tag("x="), parse::digits),
                    bytes::tag(", "),
                ),
            ),
            nom::sequence::terminated(
                nom::sequence::preceded(bytes::tag("y="), parse::digits),
                bytes::tag(", "),
            ),
            nom::sequence::terminated(
                nom::sequence::preceded(bytes::tag("z="), parse::digits),
                bytes::tag(">"),
            ),
        )),
        |(x, y, z)| Moon {
            position: Coordinates { x, y, z },
            velocity: Coordinates::ZERO,
        },
    )(input)
}
