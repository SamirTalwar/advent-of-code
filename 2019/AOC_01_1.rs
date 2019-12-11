use std::io;
use std::io::BufRead;

mod errors;

fn main() -> io::Result<()> {
    let module_masses: Vec<i32> = io::BufReader::new(io::stdin())
        .lines()
        .map(|input| input.and_then(|line| line.parse::<i32>().map_err(errors::to_io)))
        .collect::<io::Result<_>>()?;

    let fuel: i32 = module_masses.iter().map(|mass| calculate_fuel(mass)).sum();

    println!("{}", fuel);

    Ok(())
}

fn calculate_fuel(mass: &i32) -> i32 {
    mass / 3 - 2
}
