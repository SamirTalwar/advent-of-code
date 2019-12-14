use std::io;

mod digits;
mod errors;
mod intcode;

const INPUT: intcode::Code = 1;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let program = intcode::parse(&input)?;
    let device = intcode::VecDevice::new(vec![INPUT]);
    let (_, outputs) = intcode::evaluate(program, device);

    intcode::validate_test_outputs(&outputs)?;
    println!("{}", outputs[outputs.len() - 1]);

    Ok(())
}
