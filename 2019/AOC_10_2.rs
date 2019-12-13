use std::collections::HashSet;
use std::convert::TryInto;
use std::io;
use std::io::Read;
use std::iter;

mod errors;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Asteroid {
    y: i128,
    x: i128,
}

impl Asteroid {
    fn angle(&self, other: &Asteroid) -> f64 {
        let y: f64 = (other.y - self.y) as f64;
        let x: f64 = (other.x - self.x) as f64;
        modulus(
            y.atan2(x) + std::f64::consts::FRAC_PI_2,
            std::f64::consts::PI * 2.0,
        )
    }
}

struct Asteroids(HashSet<Asteroid>);

impl Asteroids {
    fn iter(&self) -> impl Iterator<Item = &Asteroid> {
        self.0.iter()
    }

    fn count_in_line_of_sight(&self, asteroid: &Asteroid) -> usize {
        self.iter()
            .filter(|other| asteroid != *other)
            .filter(|other| !self.blocked(asteroid, other))
            .count()
    }

    fn vaporize_from(&self, station: &Asteroid) -> Vec<Asteroid> {
        if self.0.is_empty() {
            Vec::new()
        } else {
            let (mut result, later): (Vec<Asteroid>, Vec<Asteroid>) = self
                .0
                .iter()
                .filter(|other| &station != other)
                .partition(|other| !self.blocked(station, other));
            result.sort_by(|a, b| station.angle(a).partial_cmp(&station.angle(b)).unwrap());
            result.append(&mut Asteroids(later.into_iter().collect()).vaporize_from(station));
            result
        }
    }

    fn blocked(&self, a: &Asteroid, b: &Asteroid) -> bool {
        let x = b.x - a.x;
        let y = b.y - a.y;
        let divisor = gcd(x.abs(), y.abs());
        (1..divisor)
            .map(|n| Asteroid {
                x: a.x + x / divisor * n,
                y: a.y + y / divisor * n,
            })
            .filter(|other| a != other && b != other)
            .any(|other| self.0.contains(&other))
    }
}

impl iter::FromIterator<Asteroid> for Asteroids {
    fn from_iter<T: IntoIterator<Item = Asteroid>>(iterator: T) -> Self {
        Asteroids(iterator.into_iter().collect())
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input)?;
    let asteroids = input
        .trim()
        .split("\n")
        .enumerate()
        .flat_map(|(y, row)| {
            row.trim()
                .chars()
                .enumerate()
                .flat_map(move |(x, cell)| match cell {
                    '#' => Some(Asteroid {
                        x: x.try_into().unwrap(),
                        y: y.try_into().unwrap(),
                    }),
                    '.' => None,
                    invalid => panic!("Invalid character: {}", invalid),
                })
        })
        .collect::<Asteroids>();

    let station = asteroids
        .iter()
        .max_by_key(|asteroid| asteroids.count_in_line_of_sight(asteroid))
        .map(|asteroid| asteroid.clone())
        .ok_or(errors::io("No asteroids!"))?;

    let vaporized_order = asteroids.vaporize_from(&station);
    let chosen = vaporized_order[199];

    println!("{}", chosen.x * 100 + chosen.y);

    Ok(())
}

fn gcd(a: i128, b: i128) -> i128 {
    iter::successors(Some((a, b)), |(a, b)| {
        if *b == 0 {
            None
        } else {
            Some((*b, *a % *b))
        }
    })
    .last()
    .unwrap()
    .0
}

fn modulus(n: f64, d: f64) -> f64 {
    if n >= 0.0 {
        n % d
    } else {
        modulus(n + d, d)
    }
}
