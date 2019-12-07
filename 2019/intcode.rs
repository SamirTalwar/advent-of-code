use core::ops::{Index, IndexMut};
use std::io;

pub type Code = usize;

#[derive(Debug, Clone)]
pub struct Codes(Vec<Code>);

impl Index<Code> for Codes {
    type Output = Code;
    fn index(&self, index: Code) -> &Code {
        &self.0[index]
    }
}

impl IndexMut<Code> for Codes {
    fn index_mut(&mut self, index: Code) -> &mut Code {
        &mut self.0[index]
    }
}

pub fn parse(input: &str) -> io::Result<Codes> {
    input
        .trim()
        .split(",")
        .map(|code| {
            code.parse::<Code>()
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
        })
        .collect::<io::Result<_>>()
        .map(Codes)
}

pub fn evaluate(mut codes: &mut Codes) {
    let mut position = 0;

    loop {
        match codes[position] {
            1 => {
                operate(&mut codes, position, |a, b| a + b);
                position += 4;
            }
            2 => {
                operate(&mut codes, position, |a, b| a * b);
                position += 4;
            }
            99 => break,
            invalid => panic!("Invalid opcode: {}", invalid),
        }
    }
}

fn operate<Op>(codes: &mut Codes, position: Code, op: Op)
where
    Op: Fn(Code, Code) -> Code,
{
    let a_position = codes[position + 1];
    let a = codes[a_position];
    let b_position = codes[position + 2];
    let b = codes[b_position];
    let destination = codes[position + 3];
    codes[destination] = op(a, b);
}
