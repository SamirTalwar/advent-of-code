use std::convert::TryInto;
use std::io;
use std::ops::{Index, IndexMut};
use std::sync::mpsc;

use super::digits;
use super::errors;

type Position = usize;

pub type Code = i32;

#[derive(Debug, Clone)]
pub struct Program(Vec<Code>);

#[derive(Debug)]
pub enum Device {
    Empty,
    Sync {
        inputs: Vec<Code>,
        outputs: Vec<Code>,
    },
    Async {
        receiver: mpsc::Receiver<Code>,
        sender: mpsc::Sender<Code>,
    },
}

pub type Channel<T> = (mpsc::Receiver<T>, mpsc::Sender<T>);

#[derive(Debug, PartialEq)]
enum Opcode {
    Add,
    Multiply,
    Save,
    Output,
    JumpIfTrue,
    JumpIfFalse,
    LessThan,
    Equals,
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

impl Device {
    pub fn empty() -> Self {
        Device::Empty
    }

    pub fn with_input(input: Code) -> Device {
        Self::with_inputs(vec![input])
    }

    pub fn with_inputs(inputs: Vec<Code>) -> Device {
        Device::Sync {
            inputs,
            outputs: Vec::new(),
        }
    }

    pub fn from_channel(channel: Channel<Code>) -> Self {
        match channel {
            (receiver, sender) => Device::Async { receiver, sender },
        }
    }

    pub fn next_input(&mut self) -> Code {
        match self {
            Device::Empty => panic!("Can't read an input from an empty device."),
            Device::Sync { inputs, .. } => inputs.remove(0),
            Device::Async { receiver, .. } => receiver.recv().unwrap(),
        }
    }

    fn output(&mut self, code: Code) {
        match self {
            Device::Empty => panic!("Can't write an output to an empty device."),
            Device::Sync { outputs, .. } => outputs.push(code),
            Device::Async { sender, .. } => sender.send(code).unwrap(),
        };
    }

    pub fn ensure_no_test_outputs(&self) -> io::Result<()> {
        match self {
            Device::Empty => panic!("An empty device doesn't have outputs."),
            Device::Async { .. } => panic!("Can't read outputs from an async device."),
            Device::Sync { outputs, .. } => {
                if outputs.len() == 1 {
                    Ok(())
                } else if outputs.len() == 0 {
                    Err(io::Error::new(
                        io::ErrorKind::Other,
                        "There were no outputs at all.",
                    ))
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("There were some test outputs.\n{:?}", outputs),
                    ))
                }
            }
        }
    }

    pub fn validate_test_outputs(&self) -> io::Result<()> {
        match self {
            Device::Empty => panic!("An empty device doesn't have outputs."),
            Device::Async { .. } => panic!("Can't read outputs from an async device."),
            Device::Sync { outputs, .. } => {
                let end = outputs.len() - 1;
                if outputs[..end].iter().all(|value| *value == 0) {
                    Ok(())
                } else {
                    Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("Test outputs were not valid.\n{:?}", outputs),
                    ))
                }
            }
        }
    }

    pub fn last_output(&self) -> Code {
        match self {
            Device::Empty => panic!("An empty device doesn't have outputs."),
            Device::Async { .. } => panic!("Can't read outputs from an async device."),
            Device::Sync { outputs, .. } => outputs[outputs.len() - 1],
        }
    }
}

impl Program {
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
                panic!("Parameters that an instruction writes to will never be in immediate mode.");
            }
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

    fn evaluate(
        &self,
        mut program: &mut Program,
        device: &mut Device,
        position: Position,
    ) -> Position {
        match self.opcode {
            Opcode::Add => self.operate(&mut program, position, |a, b| a + b),
            Opcode::Multiply => self.operate(&mut program, position, |a, b| a * b),
            Opcode::Save => {
                let input = device.next_input();
                program.set(position + 1, input, self.modes[0]);
                position + self.opcode.size()
            }
            Opcode::Output => {
                let value = program.at(position + 1, self.modes[0]);
                device.output(value);
                position + self.opcode.size()
            }
            Opcode::JumpIfTrue => self.jump_if(&mut program, position, |value| value != 0),
            Opcode::JumpIfFalse => self.jump_if(&mut program, position, |value| value == 0),
            Opcode::LessThan => {
                self.operate(&mut program, position, |a, b| if a < b { 1 } else { 0 })
            }
            Opcode::Equals => {
                self.operate(&mut program, position, |a, b| if a == b { 1 } else { 0 })
            }
            Opcode::Stop => panic!("Attempted to evaluate a Stop instruction."),
        }
    }

    fn operate<F>(&self, program: &mut Program, position: Position, operation: F) -> Position
    where
        F: Fn(Code, Code) -> Code,
    {
        let a = program.at(position + 1, self.modes[0]);
        let b = program.at(position + 2, self.modes[1]);
        program.set(position + 3, operation(a, b), self.modes[2]);
        position + self.opcode.size()
    }

    fn jump_if<F>(&self, program: &mut Program, position: Position, predicate: F) -> Position
    where
        F: Fn(Code) -> bool,
    {
        let value = program.at(position + 1, self.modes[0]);
        if predicate(value) {
            program.at(position + 2, self.modes[1]).try_into().unwrap()
        } else {
            position + self.opcode.size()
        }
    }
}

impl Index<Position> for Program {
    type Output = Code;
    fn index(&self, index: Position) -> &Code {
        &self.0[index]
    }
}

impl IndexMut<Position> for Program {
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
            5 => Opcode::JumpIfTrue,
            6 => Opcode::JumpIfFalse,
            7 => Opcode::LessThan,
            8 => Opcode::Equals,
            99 => Opcode::Stop,
            invalid => panic!("Invalid opcode: {}", invalid),
        }
    }

    fn size(&self) -> usize {
        1 + match self {
            Opcode::Add => 3,
            Opcode::Multiply => 3,
            Opcode::Save => 1,
            Opcode::Output => 1,
            Opcode::JumpIfTrue => 2,
            Opcode::JumpIfFalse => 2,
            Opcode::LessThan => 3,
            Opcode::Equals => 3,
            Opcode::Stop => 0,
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

pub fn parse(input: &str) -> io::Result<Program> {
    input
        .trim()
        .split(",")
        .map(|code| code.parse::<Code>().map_err(errors::to_io))
        .collect::<io::Result<_>>()
        .map(Program)
}

pub fn evaluate(mut program: &mut Program, mut device: &mut Device) {
    let mut position = 0;

    loop {
        let instruction = Instruction::parse(program[position]);
        if instruction.opcode == Opcode::Stop {
            break;
        }
        position = instruction.evaluate(&mut program, &mut device, position);
    }
}
