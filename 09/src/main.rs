use std::fs;

type Lines = Vec<Vec<i32>>;

// reading and parsing
fn read_line(line: &str) -> Vec<i32> {
    line.split_ascii_whitespace()
        .map(|num| num.parse().unwrap())
        .collect()
}

fn read_body(body: String) -> Lines {
    body.lines().map(read_line).collect()
}

fn read_file(path: &str) -> Lines {
    read_body(fs::read_to_string(format!("./resources/{path}.txt")).unwrap())
}

enum Time {
    Future,
    Past,
}
// actual analysis
fn next_in_sequence(sequence: &[i32], direction: &Time) -> i32 {
    let delta = match differences(sequence) {
        diffs if diffs.iter().all(|n| n == &0) => 0,
        diffs => next_in_sequence(&diffs, direction),
    };
    match direction {
        Time::Future => sequence.last().unwrap() + delta,
        Time::Past => sequence.first().unwrap() - delta,
    }
}

fn differences(sequence: &[i32]) -> Vec<i32> {
    sequence
        .windows(2)
        .map(|window| match window {
            [fst, snd] => snd - fst,
            _ => unreachable!(),
        })
        .collect()
}

fn sum_of_nexts(input: &Lines, direction: &Time) -> i32 {
    input
        .iter()
        .map(|line| next_in_sequence(line, direction))
        .sum()
}

fn main() {
    // let example = read_file("example");
    let input = read_file("input");
    println!("Part 1: {}", sum_of_nexts(&input, &Time::Future));
    println!("Part 2: {}", sum_of_nexts(&input, &Time::Past));
}
