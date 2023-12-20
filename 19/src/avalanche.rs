use std::collections::HashMap;

use crate::{
    part::Part,
    part_range::{FieldRange, PartRange},
    workflow::{Destination, Workflow},
};

#[derive(Debug)]
pub struct Avalanche {
    workflows: HashMap<String, Workflow>,
    parts: Vec<Part>,
}

impl Avalanche {
    pub fn parse(input: &str) -> Self {
        let mut ls = input.lines();
        let mut workflows = HashMap::new();
        let mut parts = Vec::new();

        for line in ls.by_ref() {
            if line.is_empty() {
                break;
            }
            let split: Vec<&str> = line.split(&['{', '}'][..]).collect();
            workflows.insert(split[0].to_string(), Workflow::parse(split[1]));
        }

        for line in ls {
            if line.is_empty() {
                break;
            }
            parts.push(Part::parse(line));
        }

        Self { workflows, parts }
    }

    pub fn sum_of_parts(&self) -> u32 {
        self.parts
            .iter()
            .filter_map(|part| match part.is_accepted(&self.workflows) {
                true => Some(part.sum_of_ratings()),
                false => None,
            })
            .sum()
    }

    pub fn accepted_combinations_of_ratings(&self) -> u64 {
        let init_field_range = FieldRange {
            start: 1,
            end: 4000,
        };
        let part_range = PartRange {
            x: init_field_range,
            m: init_field_range,
            a: init_field_range,
            s: init_field_range,
        };
        let map_in = self.workflows.get("in").unwrap();
        part_range
            .resolve(&self.workflows, map_in)
            .iter()
            .filter(|(_r, d)| matches!(d, Destination::Accepted))
            .map(|(r, _d)| r.n_possibilities())
            .sum()
    }
}
