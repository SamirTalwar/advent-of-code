use std::collections::HashMap;
use std::io;
use std::io::BufRead;

type Satellite = String;
type Orbits = HashMap<Satellite, Satellite>;
type Distance = usize;
type Distances = HashMap<Satellite, Distance>;

fn main() -> io::Result<()> {
    let orbits: Orbits = io::BufReader::new(io::stdin())
        .lines()
        .map(|input| {
            input.map(|line| {
                let splits = line.split(")").collect::<Vec<&str>>();
                (splits[1].to_string(), splits[0].to_string())
            })
        })
        .collect::<io::Result<_>>()?;

    let mut distances: Distances = HashMap::new();
    orbits.keys().for_each(|orbiter| {
        count(orbiter, &orbits, &mut distances);
    });
    println!("{}", distances.values().sum::<Distance>());

    Ok(())
}

fn count(orbiter: &Satellite, orbits: &Orbits, distances: &mut Distances) -> Distance {
    if !distances.contains_key(orbiter) {
        let distance = match orbits.get(orbiter) {
            Some(orbitee) => 1 + count(orbitee, orbits, distances),
            None => 0,
        };
        distances.insert(orbiter.clone(), distance);
    }
    distances[orbiter]
}
