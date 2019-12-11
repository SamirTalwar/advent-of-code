use std::io;
use std::sync::mpsc;
use std::thread;

use permutohedron;

mod digits;
mod intcode;

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let starting_program = intcode::parse(&input)?;

    let mut phase_values = vec![5, 6, 7, 8, 9];
    let mut largest = 0;
    permutohedron::heap_recursive(&mut phase_values, |phases| {
        let channels = wire_up_channels(
            phases
                .iter()
                .map(|phase| {
                    let channel = mpsc::channel();
                    channel.0.send(*phase).unwrap();
                    channel
                })
                .collect(),
        );
        channels[channels.len() - 1].1.send(0).unwrap();
        let outputs = channels
            .into_iter()
            .enumerate()
            .map(|(index, channel)| {
                let mut program = starting_program.clone();
                let mut device = intcode::Device::from_channel(channel);
                thread::spawn(move || {
                    intcode::evaluate(&mut program, &mut device);
                    if index == 0 {
                        device.next_input()
                    } else {
                        0
                    }
                })
            })
            .collect::<Vec<_>>()
            .into_iter()
            .map(|thread| {
                thread
                    .join()
                    .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("{:?}", err)))
            })
            .collect::<io::Result<Vec<_>>>()
            .unwrap();
        let result = outputs[0];
        largest = largest.max(result);
    });

    println!("{}", largest);

    Ok(())
}

fn wire_up_channels<T>(
    channels: Vec<(mpsc::Sender<T>, mpsc::Receiver<T>)>,
) -> Vec<intcode::Channel<T>> {
    let (mut senders, receivers): (Vec<mpsc::Sender<T>>, Vec<mpsc::Receiver<T>>) =
        channels.into_iter().unzip();
    let first_sender = senders.remove(0);
    senders.push(first_sender);
    receivers.into_iter().zip(senders).collect()
}
