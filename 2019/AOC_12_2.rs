use std::collections::HashSet;
use std::io;
use std::io::BufRead;
use std::iter;

use nom;
use nom::bytes::complete as bytes;
use nom::character::complete as character;

mod errors;
mod parse;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Moons {
    positions_x: Vec<isize>,
    positions_y: Vec<isize>,
    positions_z: Vec<isize>,
    velocities_x: Vec<isize>,
    velocities_y: Vec<isize>,
    velocities_z: Vec<isize>,
}

impl Moons {
    fn step(mut self) -> Self {
        self.velocities_x = Self::apply_gravity(&self.positions_x, self.velocities_x);
        self.velocities_y = Self::apply_gravity(&self.positions_y, self.velocities_y);
        self.velocities_z = Self::apply_gravity(&self.positions_z, self.velocities_z);
        self.positions_x = Self::apply_velocity(self.positions_x, &self.velocities_x);
        self.positions_y = Self::apply_velocity(self.positions_y, &self.velocities_y);
        self.positions_z = Self::apply_velocity(self.positions_z, &self.velocities_z);
        self
    }

    fn apply_gravity(positions: &Vec<isize>, velocities: Vec<isize>) -> Vec<isize> {
        let mut joined: Vec<(usize, (isize, isize))> = positions
            .iter()
            .cloned()
            .zip(velocities.into_iter())
            .enumerate()
            .collect();
        joined.sort_by_key(|(_, (position, _))| *position);
        let mut new_velocities: Vec<(usize, isize)> = joined
            .into_iter()
            .enumerate()
            .map(|(order, (index, (_, velocity)))| {
                (index, velocity - ((order as isize - 1) * 2 - 1))
            })
            .collect();
        new_velocities.sort_by_key(|(index, _)| *index);
        new_velocities
            .into_iter()
            .map(|(_, velocity)| velocity)
            .collect()
    }

    fn apply_velocity(positions: Vec<isize>, velocities: &Vec<isize>) -> Vec<isize> {
        positions
            .iter()
            .zip(velocities.iter())
            .map(|(position, velocity)| position + velocity)
            .collect()
    }
}

fn main() -> io::Result<()> {
    let lines: Vec<String> = io::BufReader::new(io::stdin())
        .lines()
        .collect::<io::Result<_>>()?;
    let moon_coordinates: Vec<(isize, isize, isize)> = lines
        .iter()
        .map(|line| parse::completely(parse)(line))
        .collect::<io::Result<_>>()?;
    let mut velocities = Vec::new();
    velocities.resize(moon_coordinates.len(), 0);
    let moons = Moons {
        positions_x: moon_coordinates
            .iter()
            .map(|(x, _, _)| x)
            .cloned()
            .collect(),
        positions_y: moon_coordinates
            .iter()
            .map(|(_, y, _)| y)
            .cloned()
            .collect(),
        positions_z: moon_coordinates
            .iter()
            .map(|(_, _, z)| z)
            .cloned()
            .collect(),
        velocities_x: velocities.clone(),
        velocities_y: velocities.clone(),
        velocities_z: velocities.clone(),
    };

    let mut previous: HashSet<Moons> = HashSet::new();
    let steps = iter::successors(Some(moons), |moons| Some(moons.clone().step()));
    for (i, step) in steps.enumerate() {
        println!("{}: {:?}", i, step);
        if i == 10 {
            break;
        }
        if previous.contains(&step) {
            println!("{}", i);
            break;
        }
        previous.insert(step);
    }

    Ok(())
}

fn parse(input: &str) -> nom::IResult<&str, (isize, isize, isize)> {
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
    ))(input)
}
