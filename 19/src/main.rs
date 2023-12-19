use avalanche::Avalanche;
use std::fs;

pub mod avalanche;
pub mod part;
pub mod workflow;

fn main() {
    let text = fs::read_to_string("./resources/input.txt").unwrap();
    let avalanche = Avalanche::parse(&text);
    // dbg!(avalanche);
    dbg!(avalanche.sum_of_parts());
}
