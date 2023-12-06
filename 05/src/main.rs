mod almanac;

use std::fs;

use crate::almanac::Almanac;

fn solve_for_file(file_name: &str) {
    let path = format!("./resources/{}.txt", file_name);
    let contents = fs::read_to_string(path).expect("to read file");
    let almanac = Almanac::new(&contents).unwrap();
    let seed_locations = almanac.seed_locations();
    let lowest_seed_location = seed_locations.iter().min().unwrap();
    dbg!(&seed_locations);
    dbg!(&lowest_seed_location);
}

fn main() {
    solve_for_file("example");
    solve_for_file("input");
}
