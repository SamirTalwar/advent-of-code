use std::io;

mod intcode;

fn main() -> io::Result<()> {
    let mut input: String = String::new();
    io::stdin().read_line(&mut input)?;

    let mut codes = intcode::parse(&input)?;
    codes[1] = 12;
    codes[2] = 2;
    intcode::evaluate(&mut codes);

    println!("{}", codes[0]);

    Ok(())
}
