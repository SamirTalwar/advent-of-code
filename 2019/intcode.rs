use std::convert::TryInto;
use std::io;
use std::ops::{Index, IndexMut};
use std::sync::mpsc;

use super::digits;
use super::errors;

const DEFAULT_CODE: Code = 0;

pub type Code = i128;

type Position = usize;

type RelativeBase = Code;

#[derive(Debug, Clone)]
pub struct Program(Vec<Code>);

#[derive(Debug)]
struct ProgramState {
    program: Program,
    position: Position,
    relative_base: RelativeBase,
}

pub trait Device {
    type DeviceResult;

    fn next_input(self) -> (Code, Self);

    fn output(self, code: Code) -> Self;

    fn result(self) -> Self::DeviceResult;
}

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
    AdjustRelativeBase,
    Stop,
}

#[derive(Debug, Clone, Copy)]
enum ParameterMode {
    PositionMode,
    ImmediateMode,
    RelativeMode,
}

#[derive(Debug)]
struct Instruction {
    opcode: Opcode,
    modes: Vec<ParameterMode>,
}

pub struct EmptyDevice {}

impl EmptyDevice {
    pub fn new() -> Self {
        Self {}
    }
}

impl Device for EmptyDevice {
    type DeviceResult = ();

    fn next_input(self) -> (Code, Self) {
        panic!("Can't read an input from an empty device.");
    }

    fn output(self, _code: Code) -> Self {
        panic!("Can't write an output to an empty device.");
    }

    fn result(self) -> Self::DeviceResult {
        ()
    }
}

pub struct VecDevice {
    inputs: Vec<Code>,
    outputs: Vec<Code>,
}

impl VecDevice {
    pub fn new(inputs: Vec<Code>) -> Self {
        Self {
            inputs,
            outputs: Vec::new(),
        }
    }
}

impl Device for VecDevice {
    type DeviceResult = Vec<Code>;

    fn next_input(mut self) -> (Code, Self) {
        (self.inputs.remove(0), self)
    }

    fn output(mut self, code: Code) -> Self {
        self.outputs.push(code);
        self
    }

    fn result(self) -> Self::DeviceResult {
        self.outputs
    }
}

pub struct ChannelDevice {
    receiver: mpsc::Receiver<Code>,
    sender: mpsc::Sender<Code>,
}

impl ChannelDevice {
    pub fn new(receiver: mpsc::Receiver<Code>, sender: mpsc::Sender<Code>) -> Self {
        Self { receiver, sender }
    }
}

impl Device for ChannelDevice {
    type DeviceResult = (mpsc::Receiver<Code>, mpsc::Sender<Code>);

    fn next_input(self) -> (Code, Self) {
        (self.receiver.recv().unwrap(), self)
    }

    fn output(self, code: Code) -> Self {
        self.sender.send(code).unwrap();
        self
    }

    fn result(self) -> Self::DeviceResult {
        (self.receiver, self.sender)
    }
}

pub struct CooperativeDevice<State, F>
where
    F: Copy + FnOnce(State, Code) -> (State, Vec<Code>),
{
    state: State,
    inputs: Vec<Code>,
    behavior: F,
}

impl<State, F> CooperativeDevice<State, F>
where
    F: Copy + FnOnce(State, Code) -> (State, Vec<Code>),
{
    pub fn new(starting_state: State, starting_inputs: Vec<Code>, behavior: F) -> Self {
        CooperativeDevice {
            state: starting_state,
            inputs: starting_inputs,
            behavior,
        }
    }
}

impl<State, F> Device for CooperativeDevice<State, F>
where
    F: Copy + FnOnce(State, Code) -> (State, Vec<Code>),
{
    type DeviceResult = State;

    fn next_input(mut self) -> (Code, Self) {
        (self.inputs.remove(0), self)
    }

    fn output(mut self, code: Code) -> Self {
        let (new_state, mut new_inputs) = (self.behavior)(self.state, code);
        self.state = new_state;
        self.inputs.append(&mut new_inputs);
        self
    }

    fn result(self) -> Self::DeviceResult {
        self.state
    }
}

impl Index<Position> for Program {
    type Output = Code;
    fn index(&self, index: Position) -> &Code {
        self.0.get(index).unwrap_or(&DEFAULT_CODE)
    }
}

impl IndexMut<Position> for Program {
    fn index_mut(&mut self, index: Position) -> &mut Code {
        if index >= self.0.len() {
            self.0.resize(index + 1, DEFAULT_CODE);
        }
        &mut self.0[index]
    }
}

impl ProgramState {
    fn next_instruction(&self) -> Instruction {
        Instruction::parse(self.program[self.position])
    }

    fn at(&self, position: Position, mode: ParameterMode) -> Code {
        match mode {
            ParameterMode::PositionMode => {
                let source = self.program[position];
                self.program[source.try_into().expect(&format!("source was {}", source))]
            }
            ParameterMode::ImmediateMode => self.program[position],
            ParameterMode::RelativeMode => {
                let source = self.program[position];
                self.program[(source + self.relative_base)
                    .try_into()
                    .expect(&format!("source was {}", source))]
            }
        }
    }

    fn set(&mut self, position: Position, value: Code, mode: ParameterMode) {
        match mode {
            ParameterMode::PositionMode => {
                let destination = self.program[position];
                self.program[destination
                    .try_into()
                    .expect(&format!("destination was {}", destination))] = value;
            }
            ParameterMode::ImmediateMode => {
                panic!("Parameters that an instruction writes to will never be in immediate mode.");
            }
            ParameterMode::RelativeMode => {
                let destination = self.program[position];
                self.program[(destination + self.relative_base)
                    .try_into()
                    .expect(&format!("destination was {}", destination))] = value;
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

    fn evaluate<D, R>(&self, mut state: ProgramState, device: D) -> (ProgramState, D)
    where
        D: Device<DeviceResult = R>,
    {
        match self.opcode {
            Opcode::Add => (self.operate(state, |a, b| a + b), device),
            Opcode::Multiply => (self.operate(state, |a, b| a * b), device),
            Opcode::Save => {
                let (input, new_device) = device.next_input();
                state.set(state.position + 1, input, self.modes[0]);
                state.position += self.opcode.size();
                (state, new_device)
            }
            Opcode::Output => {
                let value = state.at(state.position + 1, self.modes[0]);
                let new_device = device.output(value);
                state.position += self.opcode.size();
                (state, new_device)
            }
            Opcode::JumpIfTrue => (self.jump_if(state, |value| value != 0), device),
            Opcode::JumpIfFalse => (self.jump_if(state, |value| value == 0), device),
            Opcode::LessThan => (
                self.operate(state, |a, b| if a < b { 1 } else { 0 }),
                device,
            ),
            Opcode::Equals => (
                self.operate(state, |a, b| if a == b { 1 } else { 0 }),
                device,
            ),
            Opcode::AdjustRelativeBase => {
                state.relative_base += state.at(state.position + 1, self.modes[0]);
                state.position += self.opcode.size();
                (state, device)
            }
            Opcode::Stop => panic!("Attempted to evaluate a Stop instruction."),
        }
    }

    fn operate<F>(&self, mut state: ProgramState, operation: F) -> ProgramState
    where
        F: Fn(Code, Code) -> Code,
    {
        let a = state.at(state.position + 1, self.modes[0]);
        let b = state.at(state.position + 2, self.modes[1]);
        state.set(state.position + 3, operation(a, b), self.modes[2]);
        state.position += self.opcode.size();
        state
    }

    fn jump_if<F>(&self, mut state: ProgramState, predicate: F) -> ProgramState
    where
        F: Fn(Code) -> bool,
    {
        let value = state.at(state.position + 1, self.modes[0]);
        if predicate(value) {
            let position = state
                .at(state.position + 2, self.modes[1])
                .try_into()
                .unwrap();
            ProgramState { position, ..state }
        } else {
            state.position += self.opcode.size();
            state
        }
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
            9 => Opcode::AdjustRelativeBase,
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
            Opcode::AdjustRelativeBase => 1,
            Opcode::Stop => 0,
        }
    }
}

impl ParameterMode {
    fn parse(digit: digits::Digit) -> ParameterMode {
        match digit {
            0 => ParameterMode::PositionMode,
            1 => ParameterMode::ImmediateMode,
            2 => ParameterMode::RelativeMode,
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

pub fn evaluate<D, R>(program: Program, mut device: D) -> (Program, R)
where
    D: Device<DeviceResult = R>,
{
    let mut state = ProgramState {
        program,
        position: 0,
        relative_base: 0,
    };
    loop {
        let instruction = state.next_instruction();
        // println!("{} / {:?}", state.position, instruction);
        // println!("{:?}", state.program);
        if instruction.opcode == Opcode::Stop {
            break;
        } else {
            let (new_state, new_device) = instruction.evaluate(state, device);
            state = new_state;
            device = new_device;
            // println!("{:?}", state.program);
        }
    }
    (state.program, device.result())
}

pub fn read_parse_and_evaluate(inputs: Vec<Code>) -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let program = parse(&input)?;
    let device = VecDevice::new(inputs);
    let (_, outputs) = evaluate(program, device);

    ensure_no_test_outputs(&outputs)?;
    println!("{}", outputs[0]);

    Ok(())
}

pub fn ensure_no_test_outputs(outputs: &Vec<Code>) -> io::Result<()> {
    if outputs.len() == 1 {
        Ok(())
    } else if outputs.len() == 0 {
        Err(errors::io("There were no outputs at all."))
    } else {
        Err(errors::io(&format!(
            "There were some test outputs.\n{:?}",
            outputs
        )))
    }
}

pub fn validate_test_outputs(outputs: &Vec<Code>) -> io::Result<()> {
    let end = outputs.len() - 1;
    if outputs[..end].iter().all(|value| *value == 0) {
        Ok(())
    } else {
        Err(errors::io(&format!(
            "Test outputs were not valid.\n{:?}",
            outputs
        )))
    }
}
