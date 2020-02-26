use std::fmt;
use std::time;

pub struct Debug {
    start_time: time::Instant,
}

impl Debug {
    pub fn new() -> Self {
        Debug {
            start_time: time::Instant::now(),
        }
    }

    pub fn log<T>(&self, name: &str, value: T) -> T
    where
        T: fmt::Debug,
    {
        println!(
            "[{}s] {} = {:?}",
            self.start_time.elapsed().as_secs(),
            name,
            value
        );
        value
    }
}
