use std::io;

mod digits;
mod errors;
mod intcode;
use intcode::Code;

const EXPECTED_OUTPUT: Code = 19690720;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let starting_program = intcode::parse(&input)?;

    let program_inputs: Vec<(Code, Code)> = (0..100)
        .flat_map(move |noun| (0..100).map(move |verb| (noun, verb)))
        .collect();

    let (noun, verb) = program_inputs
        .iter()
        .find(|(noun, verb)| {
            let mut input_program = starting_program.clone();
            input_program[1] = *noun;
            input_program[2] = *verb;
            let mut device = intcode::Device::empty();
            let output_program = intcode::evaluate(input_program, &mut device);
            output_program[0] == EXPECTED_OUTPUT
        })
        .ok_or(io::ErrorKind::NotFound)?;

    println!("{}", noun * 100 + verb);

    Ok(())
}
