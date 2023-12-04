mod schematic;

use std::fs;

use crate::schematic::Schematic;

fn main() {
    let contents = fs::read_to_string("./resources/input").expect("to read file");
    let parsed = Schematic::new(&contents);
    println!("sum of part numbers: {}", parsed.sum_of_part_numbers());
}
