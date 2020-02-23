use std::fmt;
use std::io;
use std::iter;

mod errors;

type Numbers = Vec<i64>;

const PHASES: usize = 100;
const ANSWER_LENGTH: usize = 8;

fn main() -> io::Result<()> {
    let input: Numbers = {
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        input
            .trim()
            .chars()
            .map(|c| {
                c.to_digit(10)
                    .map(|d| d as i64)
                    .ok_or(errors::io(&format!("Invalid digit: {}", c)))
            })
            .collect::<io::Result<Numbers>>()
    }?;

    let result = &iter::successors(Some(input), |input| Some(phase(&input)))
        .nth(PHASES)
        .unwrap();
    let answer = {
        let mut answer = join_vec(&result);
        answer.truncate(ANSWER_LENGTH);
        answer
    };
    println!("{}", answer);

    Ok(())
}

fn phase(input: &Numbers) -> Numbers {
    (1..=input.len())
        .map(|y_offset| {
            (input
                .iter()
                .enumerate()
                .map(|(x_offset, input_value)| input_value * pattern_at(y_offset, x_offset))
                .sum::<i64>()
                % 10)
                .abs()
        })
        .collect()
}

fn pattern_at(y_offset: usize, x_offset: usize) -> i64 {
    let x = x_offset % (y_offset * 4);
    if x < y_offset - 1 {
        0
    } else if x < y_offset * 2 - 1 {
        1
    } else if x < y_offset * 3 - 1 {
        0
    } else if x < y_offset * 4 - 1 {
        -1
    } else {
        0
    }
}

fn join_vec<T>(vector: &Vec<T>) -> String
where
    T: fmt::Display,
{
    vector
        .iter()
        .map(|value| format!("{}", value))
        .collect::<Vec<String>>()
        .join("")
}
