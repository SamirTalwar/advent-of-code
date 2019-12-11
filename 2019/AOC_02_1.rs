use std::io;

mod digits;
mod errors;
mod intcode;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let mut program = intcode::parse(&input)?;
    let mut device = intcode::Device::empty();
    intcode::evaluate(&mut program, &mut device);

    println!("{}", program[0]);

    Ok(())
}
