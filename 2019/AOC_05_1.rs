use std::io;

mod digits;
mod intcode;

const INPUT: intcode::Code = 1;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let mut program = intcode::parse(&input)?;
    let mut device = intcode::Device::with_input(INPUT);
    intcode::evaluate(&mut program, &mut device);

    device.validate_test_outputs()?;
    println!("{}", device.diagnostic_code_output());

    Ok(())
}
