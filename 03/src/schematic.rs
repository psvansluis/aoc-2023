use std::{collections::HashSet, ops::Add};

#[derive(PartialEq, Debug)]
enum Coordinate {
    Digit(u8),
    Symbol(char),
    Point,
}

#[derive(Debug)]
pub struct Schematic(Vec<Vec<Coordinate>>);

impl Schematic {
    pub fn new(input: &str) -> Self {
        Self(
            input
                .lines()
                .map(|line| {
                    line.chars()
                        .map(Self::parse_char)
                        .collect::<Vec<Coordinate>>()
                })
                .collect(),
        )
    }

    fn parse_char(ch: char) -> Coordinate {
        match ch.to_digit(10) {
            Some(d) => Coordinate::Digit(d as u8),
            _ if ch == '.' => Coordinate::Point,
            _ => Coordinate::Symbol(ch),
        }
    }

    fn surrounding_symbol_indices(&self, row: usize) -> HashSet<usize> {
        (row.saturating_sub(1)..=row + 1)
            .flat_map(|r| {
                self.0
                    .get(r)
                    .map(|r| {
                        r.iter()
                            .enumerate()
                            .filter(|(_i, coord)| matches!(coord, Coordinate::Symbol(_)))
                            .map(|tuple| tuple.0)
                            .collect()
                    })
                    .unwrap_or(Vec::new())
            })
            .collect()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    fn numbers_with_pos_in_row(&self, row: usize) -> Vec<(usize, usize, u32)> {
        self.0
            .get(row)
            .map(|r| {
                r.iter()
                    .enumerate()
                    .filter_map(|(i, coord)| match coord {
                        Coordinate::Digit(d) => Some((i, *d)),
                        _ => None,
                    })
                    .fold(Vec::new(), |mut acc: Vec<(usize, usize, u32)>, (i, d)| {
                        match acc.last_mut() {
                            Some(last) if i - last.1 == 1 => {
                                *last = (last.0, i, last.2 * 10 + d as u32);
                            }
                            _ => {
                                acc.push((i, i, d as u32));
                            }
                        }
                        acc
                    })
            })
            .unwrap_or(Vec::new())
    }

    fn part_numbers_in_row(&self, row: usize) -> Vec<u32> {
        let surrounding_symbol_indices = self.surrounding_symbol_indices(row);
        let numbers_with_pos_in_row = self.numbers_with_pos_in_row(row);
        numbers_with_pos_in_row
            .into_iter()
            .filter(|(i_start, i_end, _)| {
                surrounding_symbol_indices
                    .iter()
                    .any(|i_symbol| (i_start.saturating_sub(1)..=i_end.add(1)).contains(i_symbol))
            })
            .map(|tuple| tuple.2)
            .collect()
    }

    pub fn sum_of_part_numbers(&self) -> u32 {
        (0..self.len())
            .flat_map(|i_row| self.part_numbers_in_row(i_row))
            .sum()
    }

    fn gear_indices_in_row(&self, row: usize) -> Vec<usize> {
        let Some(r) = self.0.get(row) else {
            return Vec::new();
        };
        r.iter()
            .enumerate()
            .filter(|(_, coord)| matches!(coord, Coordinate::Symbol('*')))
            .map(|tuple| tuple.0)
            .collect()
    }

    fn gear_ratios_in_row(&self, row: usize) -> Vec<u32> {
        let numbers_with_pos_in_surrounding_rows: Vec<(usize, usize, u32)> = (row.saturating_sub(1)
            ..=row + 1)
            .flat_map(|r| self.numbers_with_pos_in_row(r))
            .collect();
        self.gear_indices_in_row(row)
            .iter()
            .filter_map(|i_gear| {
                let neighbouring_numbers: Vec<(usize, usize, u32)> =
                    numbers_with_pos_in_surrounding_rows
                        .clone()
                        .into_iter()
                        .filter(|(i_start, i_end, _)| {
                            (i_start.saturating_sub(1)..=i_end.add(1)).contains(i_gear)
                        })
                        .collect();
                match neighbouring_numbers[..] {
                    [(_, _, num_a), (_, _, num_b)] => Some(num_a * num_b),
                    _ => None,
                }
            })
            .collect()
    }

    pub fn sum_of_gear_ratios(&self) -> u32 {
        (0..self.len())
            .flat_map(|row| self.gear_ratios_in_row(row))
            .sum()
    }
}
