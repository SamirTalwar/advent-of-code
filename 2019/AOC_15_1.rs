use std::collections::HashSet;
use std::collections::VecDeque;
use std::io;

mod digits;
mod errors;
mod intcode;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Position {
    x: isize,
    y: isize,
}

impl Position {
    const ZERO: Self = Self { x: 0, y: 0 };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    fn all() -> Vec<Self> {
        vec![Self::North, Self::East, Self::South, Self::West]
    }

    fn except(excluded: &Self) -> Vec<Self> {
        match excluded {
            Self::North => vec![Self::East, Self::South, Self::West],
            Self::East => vec![Self::North, Self::South, Self::West],
            Self::South => vec![Self::North, Self::East, Self::West],
            Self::West => vec![Self::North, Self::East, Self::South],
        }
    }

    fn opposite(&self) -> Self {
        match self {
            Self::North => Self::South,
            Self::East => Self::West,
            Self::South => Self::North,
            Self::West => Self::East,
        }
    }

    fn to_code(&self) -> intcode::Code {
        match self {
            Self::North => 1,
            Self::South => 2,
            Self::West => 3,
            Self::East => 4,
        }
    }

    fn advance(&self, position: &Position) -> Position {
        match self {
            Self::North => Position {
                x: position.x,
                y: position.y - 1,
            },
            Self::South => Position {
                x: position.x,
                y: position.y + 1,
            },
            Self::West => Position {
                x: position.x - 1,
                y: position.y,
            },
            Self::East => Position {
                x: position.x + 1,
                y: position.y,
            },
        }
    }
}

type Path = VecDeque<Direction>;

#[derive(Debug, Clone, Copy, PartialEq)]
enum RunResult {
    ValidPath,
    HitWall,
    Found,
}

impl RunResult {
    fn successful(&self) -> bool {
        match self {
            Self::ValidPath => true,
            Self::HitWall => false,
            Self::Found => true,
        }
    }
}

#[derive(Debug, Clone)]
struct State {
    path: Path,
    result: Option<RunResult>,
}

fn main() -> io::Result<()> {
    let mut code = String::new();
    io::stdin().read_line(&mut code)?;
    let program = intcode::parse(&code)?;

    let mut successes: HashSet<Path> = HashSet::new();
    successes.insert(VecDeque::new());
    let mut found: Option<Path> = None;
    while found.is_none() {
        let last_successes: Vec<Path> = successes.drain().collect();
        for (path, result) in step(&program, last_successes) {
            if result == RunResult::Found {
                found = Some(path);
            } else if result.successful() {
                successes.insert(path);
            }
        }
    }

    println!("{}", found.unwrap().len());

    Ok(())
}

fn step(program: &intcode::Program, successes: Vec<Path>) -> Vec<(Path, RunResult)> {
    successes
        .iter()
        .flat_map(|success| {
            let directions = match success.back() {
                None => Direction::all(),
                Some(last) => Direction::except(&last.opposite()),
            };
            directions
                .into_iter()
                .map(move |direction| {
                    let mut path = success.clone();
                    path.push_back(direction);
                    path
                })
                .filter(|path| !loops(path))
                .map(|path| {
                    let result = run(program.clone(), path.clone());
                    (path, result)
                })
        })
        .collect()
}

fn loops(path: &Path) -> bool {
    let mut positions: HashSet<Position> = HashSet::new();
    let mut position = Position::ZERO;
    positions.insert(position);
    for direction in path {
        position = direction.advance(&position);
        if positions.contains(&position) {
            return true;
        }
        positions.insert(position);
    }
    false
}

fn run(program: intcode::Program, mut path: Path) -> RunResult {
    let first = path.pop_front().unwrap();
    let device = intcode::CooperativeDevice::new(
        State { path, result: None },
        vec![first.to_code()],
        |mut state, output| match output {
            0 => {
                state.result = Some(RunResult::HitWall);
                (state, vec![])
            }
            1 => match state.path.pop_front() {
                None => {
                    state.result = Some(RunResult::ValidPath);
                    (state, vec![])
                }
                Some(first) => (state, vec![first.to_code()]),
            },
            2 => {
                if !state.path.is_empty() {
                    panic!("Found it, but we weren't done.\n{:?}", state);
                }
                state.result = Some(RunResult::Found);
                (state, vec![])
            }
            _ => panic!("Invalid output: {}", output),
        },
    );
    let (_, state) = intcode::evaluate(program.clone(), device);
    state.result.unwrap()
}
