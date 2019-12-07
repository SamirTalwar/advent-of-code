use core::ops::{Index, IndexMut};
use std::convert::TryInto;
use std::io;

use super::digits;

type Position = usize;

pub type Code = i32;

pub type Input = Code;

pub type Outputs = Vec<Code>;

#[derive(Debug, Clone)]
pub struct Codes(Vec<Code>);

#[derive(Debug)]
pub struct Program {
    pub codes: Codes,
    input: Input,
    outputs: Outputs,
}

#[derive(Debug, PartialEq)]
enum Opcode {
    Add,
    Multiply,
    Save,
    Output,
    Stop,
}

#[derive(Debug, Clone, Copy)]
enum ParameterMode {
    PositionMode,
    ImmediateMode,
}

#[derive(Debug)]
struct Instruction {
    opcode: Opcode,
    modes: Vec<ParameterMode>,
}

impl Program {
    fn output(&mut self, code: &Code) {
        self.outputs.push(*code);
    }

    pub fn validate_test_outputs(&self) -> io::Result<()> {
        let end = self.outputs.len() - 1;
        if self.outputs[..end].iter().all(|value| *value == 0) {
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::Other,
                format!("Test outputs were not valid.\n{:?}", self.outputs),
            ))
        }
    }

    pub fn diagnostic_code_output(&self) -> Code {
        self.outputs[self.outputs.len() - 1]
    }
}

impl Codes {
    fn at(&self, position: Position, mode: ParameterMode) -> Code {
        match mode {
            ParameterMode::PositionMode => {
                let source = self[position];
                self[source.try_into().unwrap()]
            }
            ParameterMode::ImmediateMode => self[position],
        }
    }

    fn set(&mut self, position: Position, value: Code, mode: ParameterMode) {
        match mode {
            ParameterMode::PositionMode => {
                let destination = self[position];
                self[destination.try_into().unwrap()] = value;
            }
            ParameterMode::ImmediateMode => {
                self[position] = value;
            }
        }
    }
}

impl Index<Position> for Codes {
    type Output = Code;
    fn index(&self, index: Position) -> &Code {
        &self.0[index]
    }
}

impl IndexMut<Position> for Codes {
    fn index_mut(&mut self, index: Position) -> &mut Code {
        &mut self.0[index]
    }
}

impl Opcode {
    fn parse(code: Code) -> Opcode {
        match code % 100 {
            1 => Opcode::Add,
            2 => Opcode::Multiply,
            3 => Opcode::Save,
            4 => Opcode::Output,
            99 => Opcode::Stop,
            invalid => panic!("Invalid opcode: {}", invalid),
        }
    }

    fn size(&self) -> usize {
        match self {
            Opcode::Add => 4,
            Opcode::Multiply => 4,
            Opcode::Save => 2,
            Opcode::Output => 2,
            Opcode::Stop => 1,
        }
    }
}

impl ParameterMode {
    fn parse(digit: digits::Digit) -> ParameterMode {
        match digit {
            0 => ParameterMode::PositionMode,
            1 => ParameterMode::ImmediateMode,
            invalid => panic!("Invalid mode: {}", invalid),
        }
    }
}

impl Instruction {
    fn parse(code: Code) -> Instruction {
        let opcode = Opcode::parse(code);
        let mut modes = digits::to_digits((code / 100) as u32)
            .iter()
            .map(|digit| ParameterMode::parse(*digit))
            .collect::<Vec<_>>();
        modes.reverse();
        modes.resize(opcode.size() - 1, ParameterMode::PositionMode);
        Instruction { opcode, modes }
    }

    fn evaluate(self, program: &mut Program, position: Position) -> Position {
        match self.opcode {
            Opcode::Add => {
                let a = program.codes.at(position + 1, self.modes[0]);
                let b = program.codes.at(position + 2, self.modes[1]);
                program.codes.set(position + 3, a + b, self.modes[2]);
            }
            Opcode::Multiply => {
                let a = program.codes.at(position + 1, self.modes[0]);
                let b = program.codes.at(position + 2, self.modes[1]);
                program.codes.set(position + 3, a * b, self.modes[2]);
            }
            Opcode::Save => {
                let destination = program.codes[position].try_into().unwrap();
                program.codes.set(destination, program.input, self.modes[0]);
            }
            Opcode::Output => {
                let value = program.codes.at(position + 1, self.modes[0]);
                program.output(&value);
            }
            Opcode::Stop => panic!("Attempted to evaluate a Stop instruction."),
        }
        position + self.opcode.size()
    }
}

pub fn program(codes: Codes) -> Program {
    program_with_input(codes, 0)
}

pub fn program_with_input(codes: Codes, input: Code) -> Program {
    Program {
        codes,
        input,
        outputs: Vec::new(),
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

pub fn evaluate(mut program: &mut Program) {
    let mut position = 0;

    loop {
        let instruction = Instruction::parse(program.codes[position]);
        if instruction.opcode == Opcode::Stop {
            break;
        }
        position = instruction.evaluate(&mut program, position);
    }
}
