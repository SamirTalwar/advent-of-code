use std::io;

mod digits;
mod errors;
mod intcode;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let input_program = intcode::parse(&input)?;
    let device = intcode::EmptyDevice::new();
    let (output_program, _) = intcode::evaluate(input_program, device);

    println!("{}", output_program[0]);

    Ok(())
}
