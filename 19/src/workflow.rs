use std::cmp::{max, min};

use crate::{
    part::Part,
    part_range::{FieldRange, PartRange},
};

#[derive(Debug)]
pub enum Destination {
    NextFlow(String),
    Accepted,
    Rejected,
}

impl Destination {
    fn from_str(input: &str) -> Self {
        match input {
            "A" => Self::Accepted,
            "R" => Self::Rejected,
            _ => Self::NextFlow(input.to_owned()),
        }
    }
}
#[derive(Debug)]
enum Condition {
    Greater,
    Less,
}
#[derive(Debug)]
struct Rule {
    category: Category,
    condition: Condition,
    limit: u32,
    destination: Destination,
}
#[derive(Debug)]
pub enum Category {
    X,
    M,
    A,
    S,
}

impl Rule {
    fn from_str(input: &str) -> Self {
        let mut by_colon = input.split(':');
        let mut left = by_colon.next().unwrap().chars();
        let category = match left.next().unwrap() {
            'x' => Category::X,
            'm' => Category::M,
            'a' => Category::A,
            's' => Category::S,
            ch => panic!("Character not recognized as category: {ch}"),
        };
        let condition = match left.next().unwrap() {
            '>' => Condition::Greater,
            '<' => Condition::Less,
            ch => panic!("Character not recognized as condition: {ch}"),
        };
        let limit = left.collect::<String>().parse().unwrap();
        let right = by_colon.next().unwrap();
        let destination = Destination::from_str(right);
        Self {
            category,
            condition,
            limit,
            destination,
        }
    }

    fn is_satisfied(&self, part: &Part) -> bool {
        let part_value = match self.category {
            Category::X => part.x,
            Category::M => part.m,
            Category::A => part.a,
            Category::S => part.s,
        };
        let op = match self.condition {
            Condition::Greater => |a, b| a > b,
            Condition::Less => |a, b| a < b,
        };
        op(part_value, self.limit)
    }

    fn apply_to_part_range(&self, part_range: &PartRange) -> (PartRange, PartRange) {
        let lim = self.limit as u64;
        let range_to_split = match self.category {
            Category::X => part_range.x,
            Category::M => part_range.m,
            Category::A => part_range.a,
            Category::S => part_range.s,
        };
        let (accepted, rejected): (FieldRange, FieldRange) = match self.condition {
            Condition::Greater => (
                FieldRange {
                    start: max(range_to_split.start, lim + 1),
                    ..range_to_split
                },
                FieldRange {
                    end: min(range_to_split.end, lim),
                    ..range_to_split
                },
            ),
            Condition::Less => (
                FieldRange {
                    end: min(range_to_split.end, lim - 1),
                    ..range_to_split
                },
                FieldRange {
                    start: max(range_to_split.start, lim),
                    ..range_to_split
                },
            ),
        };
        let new_part_range = |field_range: &FieldRange| -> PartRange {
            match self.category {
                Category::X => PartRange {
                    x: *field_range,
                    ..*part_range
                },
                Category::M => PartRange {
                    m: *field_range,
                    ..*part_range
                },
                Category::A => PartRange {
                    a: *field_range,
                    ..*part_range
                },
                Category::S => PartRange {
                    s: *field_range,
                    ..*part_range
                },
            }
        };
        (new_part_range(&accepted), new_part_range(&rejected))
    }
}

#[derive(Debug)]
pub struct Workflow {
    rules: Vec<Rule>,
    otherwise: Destination,
}

impl Workflow {
    pub fn parse(input: &str) -> Self {
        let mut split: Vec<&str> = input.split(',').collect();
        let otherwise =
            Destination::from_str(split.pop().expect("Workflow should contain final element!"));
        let rules = split.iter().map(|str| Rule::from_str(str)).collect();
        Self { otherwise, rules }
    }

    pub fn apply(&self, part: &Part) -> &Destination {
        self.rules
            .iter()
            .find(|rule| rule.is_satisfied(part))
            .map_or(&self.otherwise, |rule| &rule.destination)
    }

    pub fn apply_range(&self, part_range: &PartRange) -> Vec<(PartRange, &Destination)> {
        let mut out = Vec::new();
        let mut next_part_range = *part_range;
        for rule in &self.rules {
            let (first, second) = rule.apply_to_part_range(&next_part_range);
            out.push((first, &rule.destination));
            next_part_range = second;
        }
        out.push((next_part_range, &self.otherwise));
        out
    }
}
