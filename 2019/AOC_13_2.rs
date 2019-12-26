use std::fmt;
use std::env;
use std::io;
use std::io::Read;
use std::sync::mpsc;
use std::thread;
use std::fs;

use pancurses;
use pancurses::Input;

mod digits;
mod errors;
mod intcode;

const WIDTH: i32 = 46;

#[derive(Debug, PartialEq, Eq)]
enum Tile {
    Empty,
    Wall,
    Block,
    Paddle,
    Ball,
}

impl Tile {
    fn from_code(code: intcode::Code) -> Self {
        match code {
            0 => Self::Empty,
            1 => Self::Wall,
            2 => Self::Block,
            3 => Self::Paddle,
            4 => Self::Ball,
            _ => panic!("Invalid tile code: {}", code),
        }
    }
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Empty => ' ',
                Self::Wall => '█',
                Self::Block => 'x',
                Self::Paddle => '_',
                Self::Ball => 'o',
            }
        )
    }
}

fn main() -> io::Result<()> {
    // we need to use STDIN for the game itself, so this one is special
    let mut input_file = fs::File::open(env::args().nth(1).unwrap())?;
    let mut input = String::new();
    input_file.read_to_string(&mut input)?;

    let mut program = intcode::parse(&input.trim())?;
    program[0] = 2;
    let mut score = 0;

    let (tile_sender, tile_receiver) = mpsc::channel();
    let (input_sender, input_receiver) = mpsc::channel();
    let (output_sender, output_receiver) = mpsc::channel();
    let device = intcode::ChannelDevice::new(input_receiver, output_sender);

    let tile_handler = thread::spawn(move || {
        let mut chunk: Vec<intcode::Code> = Vec::new();
        let mut counter: usize = 0;
        loop {
            let result = output_receiver.recv();
            if result.is_err() {
                break;
            }
            let output = result.unwrap();
            counter += 1;
            if counter % 3 == 0 {
                let x = chunk[0] as i32;
                let y = chunk[1] as i32;
                if x == -1 && y == 0 {
                    score = output;
                } else {
                    tile_sender.send((x, y, Tile::from_code(output))).unwrap();
                }
                chunk.clear();
            } else {
                chunk.push(output);
            }
        }
    });

    let screen = thread::spawn(move || {
        let mut counter: usize = 0;
        let window = pancurses::initscr();
        pancurses::noecho();
        pancurses::half_delay(1);
        window.keypad(true);
        window.nodelay(true);
        loop {
            match window.getch() {
                Some(Input::KeyLeft) => {
                    input_sender.send(-1).unwrap();
                }
                Some(Input::KeyDown) => {
                    input_sender.send(0).unwrap();
                }
                Some(Input::KeyRight) => {
                    input_sender.send(1).unwrap();
                }
                Some(Input::Character('q')) => {
                    pancurses::endwin();
                    break;
                }
                _ => (),
            };
            tile_receiver.try_iter().for_each(|(x, y, tile)| {
                window.mvprintw(y, x, &format!("{}", tile));
                window.refresh();
            });
            window.mvprintw(1, WIDTH + 1, &format!("Score: {}", score));
            counter += 1;
        }
    });

    let (_, final_output_sender) = intcode::evaluate(program, device);
    screen.join().map_err(errors::debug_to_io)?;
    drop(final_output_sender);
    tile_handler.join().map_err(errors::debug_to_io)?;

    println!("Score: {}", score);

    Ok(())
}
