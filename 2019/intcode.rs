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

    fn next_input(&mut self) -> Code;

    fn output(&mut self, code: Code);

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

    fn next_input(&mut self) -> Code {
        panic!("Can't read an input from an empty device.");
    }

    fn output(&mut self, _code: Code) {
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

    fn next_input(&mut self) -> Code {
        self.inputs.remove(0)
    }

    fn output(&mut self, code: Code) {
        self.outputs.push(code);
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

    fn next_input(&mut self) -> Code {
        self.receiver.recv().unwrap()
    }

    fn output(&mut self, code: Code) {
        self.sender.send(code).unwrap();
    }

    fn result(self) -> Self::DeviceResult {
        (self.receiver, self.sender)
    }
}

pub struct CooperativeDevice<F>
where
    F: FnMut(Code) -> Vec<Code>,
{
    inputs: Vec<Code>,
    behavior: F,
}

impl<F> CooperativeDevice<F>
where
    F: FnMut(Code) -> Vec<Code>,
{
    pub fn new(starting_inputs: Vec<Code>, behavior: F) -> Self {
        CooperativeDevice {
            inputs: starting_inputs,
            behavior,
        }
    }
}

impl<F> Device for CooperativeDevice<F>
where
    F: FnMut(Code) -> Vec<Code>,
{
    type DeviceResult = ();

    fn next_input(&mut self) -> Code {
        self.inputs.remove(0)
    }

    fn output(&mut self, code: Code) {
        let mut new_inputs = (self.behavior)(code);
        self.inputs.append(&mut new_inputs)
    }

    fn result(self) -> Self::DeviceResult {
        ()
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

    fn evaluate<D, R>(&self, state: &mut ProgramState, device: &mut D)
    where
        D: Device<DeviceResult = R>,
    {
        match self.opcode {
            Opcode::Add => self.operate(state, |a, b| a + b),
            Opcode::Multiply => self.operate(state, |a, b| a * b),
            Opcode::Save => {
                let input = device.next_input();
                state.set(state.position + 1, input, self.modes[0]);
                state.position += self.opcode.size();
            }
            Opcode::Output => {
                let value = state.at(state.position + 1, self.modes[0]);
                device.output(value);
                state.position += self.opcode.size();
            }
            Opcode::JumpIfTrue => self.jump_if(state, |value| value != 0),
            Opcode::JumpIfFalse => self.jump_if(state, |value| value == 0),
            Opcode::LessThan => self.operate(state, |a, b| if a < b { 1 } else { 0 }),
            Opcode::Equals => self.operate(state, |a, b| if a == b { 1 } else { 0 }),
            Opcode::AdjustRelativeBase => {
                state.relative_base += state.at(state.position + 1, self.modes[0]);
                state.position += self.opcode.size();
            }
            Opcode::Stop => panic!("Attempted to evaluate a Stop instruction."),
        }
    }

    fn operate<F>(&self, state: &mut ProgramState, operation: F)
    where
        F: Fn(Code, Code) -> Code,
    {
        let a = state.at(state.position + 1, self.modes[0]);
        let b = state.at(state.position + 2, self.modes[1]);
        state.set(state.position + 3, operation(a, b), self.modes[2]);
        state.position += self.opcode.size();
    }

    fn jump_if<F>(&self, state: &mut ProgramState, predicate: F)
    where
        F: Fn(Code) -> bool,
    {
        let value = state.at(state.position + 1, self.modes[0]);
        if predicate(value) {
            state.position = state
                .at(state.position + 2, self.modes[1])
                .try_into()
                .unwrap();
        } else {
            state.position += self.opcode.size();
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
            instruction.evaluate(&mut state, &mut device);
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
