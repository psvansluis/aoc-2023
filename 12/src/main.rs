use std::fs;

use crate::record::Record;

pub mod record;

fn solve_for_file(file_name: &str) {
    let path = format!("./resources/{}.txt", file_name);
    let contents = fs::read_to_string(path).expect("to read file");
    //  parse file

    let records: Vec<Record> = contents.lines().filter_map(Record::from_line).collect();
    dbg!(&records);
    println!("Solution for {}", file_name);
}

fn main() {
    solve_for_file("example");
}
