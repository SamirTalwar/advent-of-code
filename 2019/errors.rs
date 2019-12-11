use std::error;
use std::fmt;
use std::io;
use std::marker;

pub fn io(message: &str) -> io::Error {
    io::Error::new(io::ErrorKind::Other, message)
}

pub fn to_io<E>(err: E) -> io::Error
where
    E: Into<Box<dyn error::Error + marker::Send + marker::Sync>>,
{
    io::Error::new(io::ErrorKind::Other, err)
}

pub fn debug_to_io<E>(err: E) -> io::Error
where
    E: fmt::Debug,
{
    io::Error::new(io::ErrorKind::Other, format!("{:?}", err))
}
