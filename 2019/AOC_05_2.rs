use std::io;

mod digits;
mod errors;
mod intcode;

const INPUT: intcode::Code = 5;

fn main() -> io::Result<()> {
    intcode::read_parse_and_evaluate(vec![INPUT])
}
