use std::io;

type Code = usize;
type Codes = Vec<Code>;

const EXPECTED_OUTPUT: Code = 19690720;

fn main() -> io::Result<()> {
    let mut input: String = String::new();
    io::stdin().read_line(&mut input)?;

    let starting_codes: Codes = input
        .trim()
        .split(",")
        .map(|code| {
            code.parse::<Code>()
                .map_err(|err| io::Error::new(io::ErrorKind::Other, err))
        })
        .collect::<io::Result<_>>()?;

    let program_inputs: Vec<(Code, Code)> = (0..100)
        .flat_map(move |noun| (0..100).map(move |verb| (noun, verb)))
        .collect();

    let (noun, verb) = program_inputs
        .iter()
        .find(|(noun, verb)| {
            let mut codes = starting_codes.clone();
            codes[1] = *noun;
            codes[2] = *verb;

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

            codes[0] == EXPECTED_OUTPUT
        })
        .ok_or(io::ErrorKind::NotFound)?;

    println!("{}", noun * 100 + verb);

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
