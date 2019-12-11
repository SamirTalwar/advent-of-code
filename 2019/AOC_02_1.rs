use std::io;

mod digits;
mod errors;
mod intcode;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let input_program = intcode::parse(&input)?;
    let mut device = intcode::Device::empty();
    let output_program = intcode::evaluate(input_program, &mut device);

    println!("{}", output_program[0]);

    Ok(())
}
