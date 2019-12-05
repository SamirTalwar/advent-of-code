use std::io;

type Code = usize;
type Codes = Vec<Code>;

fn main() -> io::Result<()> {
    let mut input: String = String::new();
    io::stdin().read_line(&mut input)?;

    let mut codes: Codes = input
        .trim()
        .split(",")
        .map(|code| {
            code.parse::<Code>()
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
        })
        .collect::<io::Result<_>>()?;

    codes[1] = 12;
    codes[2] = 2;

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

    println!("{}", codes[0]);

    Ok(())
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
