use std::io;

mod digits;
mod errors;
mod intcode;

fn main() -> io::Result<()> {
    intcode::read_parse_and_evaluate(vec![2])
}
