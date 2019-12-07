use std::io;

mod digits;
mod intcode;
use intcode::Code;

const EXPECTED_OUTPUT: Code = 19690720;

fn main() -> io::Result<()> {
    let mut input: String = String::new();
    io::stdin().read_line(&mut input)?;

    let starting_codes = intcode::parse(&input)?;

    let program_inputs: Vec<(Code, Code)> = (0..100)
        .flat_map(move |noun| (0..100).map(move |verb| (noun, verb)))
        .collect();

    let (noun, verb) = program_inputs
        .iter()
        .find(|(noun, verb)| {
            let mut codes = starting_codes.clone();
            codes[1] = *noun;
            codes[2] = *verb;
            let mut program = intcode::program(codes);
            intcode::evaluate(&mut program);
            program.codes[0] == EXPECTED_OUTPUT
        })
        .ok_or(io::ErrorKind::NotFound)?;

    println!("{}", noun * 100 + verb);

    Ok(())
}
