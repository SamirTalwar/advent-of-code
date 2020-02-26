use std::cmp;
use std::fmt;
use std::io;
use std::iter;

use rayon::prelude::*;

mod digits;
mod errors;

type Value = i64;
type Numbers = Vec<Value>;
type Phases = Vec<Vec<(usize, usize, PhaseValue)>>;

#[derive(Debug, Clone, Copy)]
enum PhaseValue {
    Positive,
    Negative,
}

const REPETITIONS: usize = 10000;
const PHASES: usize = 100;
const OFFSET_LENGTH: usize = 7;
const ANSWER_LENGTH: usize = 8;

fn main() -> io::Result<()> {
    let input: Numbers = {
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let first_input = input
            .trim()
            .chars()
            .map(|c| {
                c.to_digit(10)
                    .map(|d| d as Value)
                    .ok_or(errors::io(&format!("Invalid digit: {}", c)))
            })
            .collect::<io::Result<Vec<Value>>>()?;
        first_input
            .iter()
            .cloned()
            .cycle()
            .take(first_input.len() * REPETITIONS)
            .collect::<Numbers>()
    };

    let offset = digits::from_digits::<usize>(
        input
            .iter()
            .cloned()
            .map(|d| d as digits::Digit)
            .take(OFFSET_LENGTH)
            .collect(),
    );

    let size = input.len();
    let phase_sequence: Vec<Option<PhaseValue>> = vec![
        None,
        Some(PhaseValue::Positive),
        None,
        Some(PhaseValue::Negative),
    ];

    let phases: Phases = (1..=size)
        .into_par_iter()
        .map(|y| {
            phase_sequence
                .iter()
                .cycle()
                .zip((0..).map(|n| n * y))
                .filter_map(|(phase, n)| {
                    phase.map(|phase| (cmp::max(n - 1, 0), cmp::min(n - 1 + y, size), phase))
                })
                .take_while(|(start, _, _)| *start < size)
                .collect()
        })
        .collect();

    let result = iter::successors(Some(input), |input| Some(phase(input, &phases)))
        .nth(PHASES)
        .unwrap();
    let full_answer = join_vec(&result);
    let answer = full_answer[offset..(offset + ANSWER_LENGTH)].to_string();
    println!("{}", answer);

    Ok(())
}

fn phase(input: &Numbers, phases: &Phases) -> Numbers {
    (0..input.len())
        .into_par_iter()
        .map(|phase| {
            phases[phase]
                .par_iter()
                .map(|(offset_start, offset_end, phase_value)| {
                    let phase_sum: Value = input[*offset_start..*offset_end].iter().sum();
                    match phase_value {
                        PhaseValue::Positive => phase_sum,
                        PhaseValue::Negative => -phase_sum,
                    }
                })
                .sum::<Value>()
        })
        .map(|result| (result % 10).abs())
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
