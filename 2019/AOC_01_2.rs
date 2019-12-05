use std::io;
use std::io::BufRead;
use std::iter;

type Mass = i32;
type Fuel = i32;

fn main() -> io::Result<()> {
    let module_masses: Vec<Mass> = io::BufReader::new(io::stdin())
        .lines()
        .map(|input| {
            input.and_then(|line| {
                line.parse::<Mass>()
                    .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
            })
        })
        .collect::<io::Result<_>>()?;

    let total_fuel: Fuel = module_masses
        .iter()
        .map(|mass| {
            let initial_fuel = calculate_fuel(mass);
            let fuel_chunks = iter::successors(Some(initial_fuel), |fuel_mass| {
                Some(calculate_fuel(fuel_mass)).filter(|value| value > &0)
            });
            fuel_chunks.sum::<Fuel>()
        })
        .sum();

    println!("{}", total_fuel);

    Ok(())
}

fn calculate_fuel(mass: &i32) -> i32 {
    i32::max(mass / 3 - 2, 0)
}
