use std::collections::HashSet;
use std::convert::TryInto;
use std::io;
use std::io::Read;
use std::iter;

mod errors;
mod numbers;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Asteroid {
    y: i128,
    x: i128,
}

struct Asteroids(HashSet<Asteroid>);

impl Asteroids {
    fn iter(&self) -> impl Iterator<Item = &Asteroid> {
        self.0.iter()
    }

    fn print_field_from_perspective_of(&self, asteroid: &Asteroid) {
        let width = self.0.iter().map(|a| a.x).max().expect("No asteroids!") + 1;
        let height = self.0.iter().map(|a| a.y).max().expect("No asteroids!") + 1;
        for y in 0..height {
            for x in 0..width {
                let other = Asteroid { x, y };
                if asteroid == &other {
                    print!("#");
                } else if self.0.contains(&other) {
                    if self.blocked(asteroid, &other) {
                        print!("%");
                    } else {
                        print!("*");
                    }
                } else {
                    print!(".");
                }
            }
            println!();
        }
    }

    fn count_in_line_of_sight(&self, asteroid: &Asteroid) -> usize {
        self.iter()
            .filter(|other| asteroid != *other)
            .filter(|other| !self.blocked(asteroid, other))
            .count()
    }

    fn blocked(&self, a: &Asteroid, b: &Asteroid) -> bool {
        let x = b.x - a.x;
        let y = b.y - a.y;
        let divisor = numbers::gcd(x.abs(), y.abs());
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

    let (best, count) = asteroids
        .iter()
        .map(|asteroid| (asteroid, asteroids.count_in_line_of_sight(asteroid)))
        .max_by_key(|(_, count)| *count)
        .ok_or(errors::io("No asteroids!"))?;

    asteroids.print_field_from_perspective_of(best);
    println!();
    println!("{}", count);

    Ok(())
}
