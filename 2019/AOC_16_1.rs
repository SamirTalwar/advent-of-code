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
    let length = input.len();

    let patterns: Vec<Numbers> = (1..=length)
        .map(|i| {
            iter::repeat(0)
                .take(i)
                .chain(iter::repeat(1).take(i))
                .chain(iter::repeat(0).take(i))
                .chain(iter::repeat(-1).take(i))
                .cycle()
                .skip(1)
                .take(length)
                .collect()
        })
        .collect();

    let result = &iter::successors(Some(input), |input| Some(phase(&input, &patterns)))
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

fn phase(input: &Numbers, patterns: &Vec<Numbers>) -> Numbers {
    patterns
        .iter()
        .map(|pattern| {
            (pattern
                .iter()
                .zip(input.iter())
                .map(|(pattern_value, input_value)| pattern_value * input_value)
                .sum::<i64>()
                % 10)
                .abs()
        })
        .collect()
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
