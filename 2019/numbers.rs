use std::iter;

use num;

pub fn gcd<N>(a: N, b: N) -> N
where
    N: num::Num + Copy,
{
    let zero = num::zero();
    iter::successors(Some((a, b)), |(a, b)| {
        if *b == zero {
            None
        } else {
            Some((*b, *a % *b))
        }
    })
    .last()
    .unwrap()
    .0
}

pub fn lcm<N>(a: N, b: N) -> N
where
    N: num::Num + Copy,
{
    a * b / gcd(a, b)
}
