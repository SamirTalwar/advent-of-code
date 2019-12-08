use std::collections::HashMap;
use std::io;
use std::io::BufRead;
use std::iter;

type Satellite = String;
type Orbits = HashMap<Satellite, Satellite>;
type Distance = usize;

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

    let transfers = distance("YOU", "SAN", &orbits) - 2;
    println!("{}", transfers);

    Ok(())
}

fn distance(start: &str, end: &str, orbits: &Orbits) -> Distance {
    let start_ancestors = ancestors(start, orbits).collect::<Vec<_>>();
    let end_ancestors = ancestors(end, orbits).collect::<Vec<_>>();
    let common_ancestor = start_ancestors
        .iter()
        .find(|ancestor| end_ancestors.contains(ancestor))
        .unwrap();
    let distance_from_start_to_common_ancestor = start_ancestors
        .iter()
        .position(|ancestor| ancestor == common_ancestor)
        .unwrap()
        + 1;
    let distance_from_common_ancestor_to_end = end_ancestors
        .iter()
        .position(|ancestor| ancestor == common_ancestor)
        .unwrap()
        + 1;
    return distance_from_start_to_common_ancestor + distance_from_common_ancestor_to_end;
}

fn ancestors(satellite: &str, orbits: &Orbits) -> Box<dyn Iterator<Item = Satellite>> {
    match orbits.get(satellite) {
        Some(next) => Box::new(iter::once(next.to_string()).chain(ancestors(next, orbits))),
        None => Box::new(iter::empty()),
    }
}
