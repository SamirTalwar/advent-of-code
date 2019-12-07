use std::io;

mod digits;
mod intcode;

fn main() -> io::Result<()> {
    let mut input: String = String::new();
    io::stdin().read_line(&mut input)?;

    let mut codes = intcode::parse(&input)?;
    codes[1] = 12;
    codes[2] = 2;
    let mut program = intcode::program(codes);
    intcode::evaluate(&mut program);

    println!("{}", program.codes[0]);

    Ok(())
}
