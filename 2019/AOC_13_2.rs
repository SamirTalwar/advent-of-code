use std::cmp::Ordering;
use std::env;
use std::fmt;
use std::fs;
use std::io;
use std::io::Read;
use std::sync::atomic;
use std::sync::atomic::AtomicUsize;
use std::sync::mpsc;
use std::sync::Arc;
use std::thread;
use std::time;

use pancurses;
use pancurses::Input;

mod digits;
mod errors;
mod intcode;

const WIDTH: i32 = 46;
const HEIGHT: i32 = 46;

const DEFAULT_FREQUENCY: u64 = 50;

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

#[derive(Debug, PartialEq, Eq)]
enum Mode {
    Automatic,
    Manual,
}

fn main() -> io::Result<()> {
    // in manual mode, we need to use STDIN for the game itself, so this one is special
    let args: Vec<String> = env::args().collect();
    let mut mode = Mode::Automatic;
    let mut input = String::new();
    if args.len() == 1 {
        io::stdin().read_to_string(&mut input)?;
    } else if args.len() == 2 {
        mode = Mode::Manual;
        let mut input_file = fs::File::open(&args[1])?;
        input_file.read_to_string(&mut input)?;
    } else {
        panic!("Invalid arguments.");
    }

    let mut program = intcode::parse(&input.trim())?;
    program[0] = 2;
    let score = Arc::new(AtomicUsize::new(0));

    let (tile_sender, tile_receiver) = mpsc::channel();
    let (input_sender, input_receiver) = mpsc::channel();
    let (output_sender, output_receiver) = mpsc::channel();
    let device = intcode::ChannelDevice::new(input_receiver, output_sender);

    let tile_handler = spawn_tile_handler(Arc::clone(&score), output_receiver, tile_sender);

    let player = match mode {
        Mode::Automatic => spawn_automatic_player(Arc::clone(&score), tile_receiver, input_sender),
        Mode::Manual => spawn_manual_player(Arc::clone(&score), tile_receiver, input_sender),
    };

    let (_, final_output_sender) = intcode::evaluate(program, device);
    player.join().map_err(errors::debug_to_io)?;
    drop(final_output_sender);
    tile_handler.join().map_err(errors::debug_to_io)?;

    println!("Score: {}", score.load(atomic::Ordering::SeqCst));

    Ok(())
}

fn spawn_tile_handler(
    score: Arc<AtomicUsize>,
    output_receiver: mpsc::Receiver<intcode::Code>,
    tile_sender: mpsc::Sender<(i32, i32, Tile)>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || {
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
                    score.store(output as usize, atomic::Ordering::SeqCst);
                } else {
                    tile_sender.send((x, y, Tile::from_code(output))).unwrap();
                }
                chunk.clear();
            } else {
                chunk.push(output);
            }
        }
    })
}

fn spawn_automatic_player(
    score: Arc<AtomicUsize>,
    tile_receiver: mpsc::Receiver<(i32, i32, Tile)>,
    input_sender: mpsc::Sender<intcode::Code>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let mut ball_position = 0;
        let mut paddle_position = 0;

        let window = pancurses::initscr();
        window.nodelay(true);
        pancurses::noecho();
        loop {
            thread::sleep(time::Duration::from_millis(DEFAULT_FREQUENCY));

            tile_receiver.try_iter().for_each(|(x, y, tile)| {
                window.mvprintw(y, x, &format!("{}", tile));
                if tile == Tile::Ball {
                    ball_position = x;
                }
                if tile == Tile::Paddle {
                    paddle_position = x;
                }
            });
            window.mvprintw(
                1,
                WIDTH + 1,
                &format!("Score: {}", score.load(atomic::Ordering::SeqCst)),
            );
            window.mv(HEIGHT, 0);
            window.refresh();

            let direction = match paddle_position.cmp(&ball_position) {
                Ordering::Less => 1,
                Ordering::Equal => 0,
                Ordering::Greater => -1,
            };
            input_sender.send(direction).unwrap();
        }
    })
}

fn spawn_manual_player(
    score: Arc<AtomicUsize>,
    tile_receiver: mpsc::Receiver<(i32, i32, Tile)>,
    input_sender: mpsc::Sender<intcode::Code>,
) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        let window = pancurses::initscr();
        pancurses::noecho();
        pancurses::half_delay(1);
        window.keypad(true);
        window.nodelay(true);
        loop {
            tile_receiver.try_iter().for_each(|(x, y, tile)| {
                window.mvprintw(y, x, &format!("{}", tile));
            });
            window.mvprintw(
                1,
                WIDTH + 1,
                &format!("Score: {}", score.load(atomic::Ordering::SeqCst)),
            );
            window.mv(HEIGHT, 0);
            window.refresh();

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
        }
    })
}
