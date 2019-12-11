use std::io;

use permutohedron;

mod digits;
mod errors;
mod intcode;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let starting_program = intcode::parse(&input)?;

    let mut phase_values = vec![0, 1, 2, 3, 4];
    let mut largest = 0;
    permutohedron::heap_recursive(&mut phase_values, |phases| {
        let result = phases.iter().fold(0, |input, phase| {
            let program = starting_program.clone();
            let mut device = intcode::Device::with_inputs(vec![*phase, input]);
            intcode::evaluate(program, &mut device);
            device.last_output()
        });
        largest = largest.max(result);
    });

    println!("{}", largest);

    Ok(())
}
