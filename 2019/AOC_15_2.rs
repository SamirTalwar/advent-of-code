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

    let prefix = VecDeque::new();

    let mut successes: HashSet<Path> = HashSet::new();
    successes.insert(VecDeque::new());
    let mut found: Option<Path> = None;
    while found.is_none() {
        let last_successes: Vec<Path> = successes.drain().collect();
        last_successes
            .iter()
            .flat_map(|success| step(&program, &prefix, success))
            .filter(|(_, result)| result.successful())
            .for_each(|(path, result)| {
                if result == RunResult::Found {
                    found = Some(path);
                } else {
                    successes.insert(path);
                }
            });
    }

    let path_to_oxygen_tank = found.unwrap();
    successes.clear();
    successes.insert(VecDeque::new());
    let mut max_distance = 0;
    while !successes.is_empty() {
        let last_successes: Vec<Path> = successes.drain().collect();
        last_successes
            .iter()
            .flat_map(|success| step(&program, &path_to_oxygen_tank, success))
            .filter(|(_, result)| result.successful())
            .for_each(|(path, _)| {
                if path.len() > max_distance {
                    max_distance = path.len();
                }
                successes.insert(path);
            });
    }

    println!("{:?}", max_distance);

    Ok(())
}

fn step<'a>(
    program: &'a intcode::Program,
    prefix: &'a Path,
    success: &'a Path,
) -> impl Iterator<Item = (Path, RunResult)> + 'a {
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
        .map(move |path| {
            let full_path = prefix
                .iter()
                .cloned()
                .chain(path.iter().cloned())
                .collect::<Path>();
            let result = run(program.clone(), full_path);
            (path, result)
        })
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
            2 => match state.path.pop_front() {
                None => {
                    state.result = Some(RunResult::Found);
                    (state, vec![])
                }
                Some(first) => (state, vec![first.to_code()]),
            },
            _ => panic!("Invalid output: {}", output),
        },
    );
    let (_, state) = intcode::evaluate(program.clone(), device);
    state.result.unwrap()
}
