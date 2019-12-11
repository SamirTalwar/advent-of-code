use std::io;

mod digits;
mod errors;

const IMAGE_WIDTH: usize = 25;
const IMAGE_HEIGHT: usize = 6;

type Pixels = Vec<Color>;

#[derive(Debug)]
struct Image {
    width: usize,
    height: usize,
    pixels: Pixels,
}

impl Image {
    fn of_size(width: usize, height: usize) -> Self {
        let size = width * height;
        let mut pixels = Vec::new();
        pixels.resize(size, Color::Transparent);
        Image {
            width,
            height,
            pixels,
        }
    }

    fn print(&self) {
        self.pixels.chunks(self.width).for_each(|row| {
            row.iter().for_each(|pixel| pixel.print());
            println!();
        });
    }
}

struct Layer<'a>(&'a [Color]);

impl Layer<'_> {
    fn apply_to(&self, image: Image) -> Image {
        let pixels = self
            .0
            .iter()
            .zip(image.pixels.into_iter())
            .map(|(layer_pixel, image_pixel)| layer_pixel.paint_onto(image_pixel))
            .collect();
        Image { pixels, ..image }
    }
}

#[derive(Debug, Clone, Copy)]
enum Color {
    Black,
    White,
    Transparent,
}

impl Color {
    fn decode(value: u32) -> Self {
        match value {
            0 => Self::Black,
            1 => Self::White,
            2 => Self::Transparent,
            _ => panic!("Invalid color value: {}", value),
        }
    }

    fn paint_onto(&self, other: Self) -> Self {
        match (self, other) {
            (Self::Black, _) => Self::Black,
            (Self::White, _) => Self::White,
            (Self::Transparent, other) => other,
        }
    }

    fn print(&self) {
        match self {
            Self::Black => print!(" "),
            Self::White => print!("✦"),
            Self::Transparent => panic!("Can't print transparent colors!"),
        }
    }
}

fn main() -> io::Result<()> {
    let mut input = String::new();
    io::stdin().read_line(&mut input)?;

    let image_size = IMAGE_WIDTH * IMAGE_HEIGHT;

    let pixels: Pixels = input
        .trim()
        .chars()
        .map(|c| {
            c.to_digit(10)
                .map(Color::decode)
                .ok_or(errors::io(&format!("Invalid digit: {}", c)))
        })
        .collect::<io::Result<_>>()?;

    let image = pixels
        .chunks(image_size)
        .rev()
        .fold(Image::of_size(IMAGE_WIDTH, IMAGE_HEIGHT), |image, layer| {
            Layer(layer).apply_to(image)
        });

    image.print();

    Ok(())
}
