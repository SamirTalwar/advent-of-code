use std::io;

mod digits;
mod intcode;
use intcode::Code;

const INPUT: Code = 1;

fn main() -> io::Result<()> {
    let mut input: String = String::new();
    io::stdin().read_line(&mut input)?;

    let codes = intcode::parse(&input)?;
    let mut program = intcode::program_with_input(codes, INPUT);
    intcode::evaluate(&mut program);

    program.validate_test_outputs()?;
    println!("{}", program.diagnostic_code_output());

    Ok(())
}
