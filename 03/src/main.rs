mod schematic;

use std::fs;

use crate::schematic::Schematic;

fn solve_for_file(file_name: &str) {
    let path = format!("./resources/{}.txt", file_name);
    let contents = fs::read_to_string(path).expect("to read file");
    let parsed = Schematic::new(&contents);
    println!("Solution for {}", file_name);
    println!("Sum of part numbers: {}", parsed.sum_of_part_numbers());
    println!("Sum of gear ratios: {}", parsed.sum_of_gear_ratios());
}

fn main() {
    solve_for_file("example");
    println!("----");
    solve_for_file("input");
}
