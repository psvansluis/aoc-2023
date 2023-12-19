use std::collections::HashMap;

use crate::{part::Part, workflow::Workflow};

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

        while let Some(line) = ls.next() {
            if line.is_empty() {
                break;
            }
            let split: Vec<&str> = line.split(&['{', '}'][..]).collect();
            workflows.insert(split[0].to_string(), Workflow::parse(split[1]));
        }

        while let Some(line) = ls.next() {
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
}
