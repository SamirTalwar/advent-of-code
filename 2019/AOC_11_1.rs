use std::collections::HashMap;
use std::io;

mod digits;
mod errors;
mod intcode;

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

impl Direction {
    fn turn(&self, turning: Turning) -> Self {
        match (self, turning) {
            (Direction::Up, Turning::Left) => Direction::Left,
            (Direction::Up, Turning::Right) => Direction::Right,
            (Direction::Right, Turning::Left) => Direction::Up,
            (Direction::Right, Turning::Right) => Direction::Down,
            (Direction::Down, Turning::Left) => Direction::Right,
            (Direction::Down, Turning::Right) => Direction::Left,
            (Direction::Left, Turning::Left) => Direction::Down,
            (Direction::Left, Turning::Right) => Direction::Up,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Turning {
    Left,
    Right,
}

impl Turning {
    fn from_code(code: intcode::Code) -> Self {
        match code {
            0 => Turning::Left,
            1 => Turning::Right,
            _ => panic!("Invalid code: {}", code),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Coordinates {
    x: i64,
    y: i64,
}

impl Coordinates {
    fn advance(&self, direction: Direction) -> Coordinates {
        match direction {
            Direction::Up => Coordinates {
                x: self.x,
                y: self.y - 1,
            },
            Direction::Right => Coordinates {
                x: self.x + 1,
                y: self.y,
            },
            Direction::Down => Coordinates {
                x: self.x,
                y: self.y + 1,
            },
            Direction::Left => Coordinates {
                x: self.x - 1,
                y: self.y,
            },
        }
    }
}

#[derive(Debug)]
struct Robot {
    direction: Direction,
    location: Coordinates,
}

impl Robot {
    fn turn(&mut self, turning: Turning) {
        self.direction = self.direction.turn(turning);
    }

    fn move_forward(&mut self) {
        self.location = self.location.advance(self.direction);
    }
}

#[derive(Debug)]
enum Color {
    Black,
    White,
}

impl Color {
    fn from_code(code: intcode::Code) -> Self {
        match code {
            0 => Color::Black,
            1 => Color::White,
            _ => panic!("Invalid code: {}", code),
        }
    }

    fn to_code(&self) -> intcode::Code {
        match self {
            Color::Black => 0,
            Color::White => 1,
        }
    }
}

type Panels = HashMap<Coordinates, Color>;

enum DeviceMode {
    Paint,
    Move,
}

fn main() -> io::Result<()> {
    let mut code = String::new();
    io::stdin().read_line(&mut code)?;

    let mut robot = Robot {
        direction: Direction::Up,
        location: Coordinates { x: 0, y: 0 },
    };

    let mut panels: Panels = HashMap::new();

    let mut device_mode = DeviceMode::Paint;
    let device =
        intcode::CooperativeDevice::new(vec![Color::Black.to_code()], |output| match device_mode {
            DeviceMode::Paint => {
                let new_color = Color::from_code(output);
                panels.insert(robot.location, new_color);

                device_mode = DeviceMode::Move;
                vec![]
            }
            DeviceMode::Move => {
                robot.turn(Turning::from_code(output));
                robot.move_forward();

                let old_color = panels.get(&robot.location).unwrap_or(&Color::Black);

                device_mode = DeviceMode::Paint;
                vec![old_color.to_code()]
            }
        });

    let program = intcode::parse(&code)?;
    intcode::evaluate(program, device);

    println!("{}", panels.len());

    Ok(())
}
