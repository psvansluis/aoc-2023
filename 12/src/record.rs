#[derive(Debug)]
pub enum Condition {
    Operational,
    Damaged,
    Unknown,
}

#[derive(Debug)]
pub struct Record {
    conditions: Vec<Condition>,
    groups: Vec<u8>,
}

impl Record {
    pub fn from_line(line: &str) -> Option<Self> {
        let mut split = line.split_ascii_whitespace();
        let conditions = Self::parse_conditions(split.next()?);
        let groups = Self::parse_groups(split.next()?);
        Some(Self { conditions, groups })
    }

    fn parse_conditions(symbols: &str) -> Vec<Condition> {
        symbols
            .chars()
            .filter_map(|ch| match ch {
                '#' => Some(Condition::Damaged),
                '.' => Some(Condition::Operational),
                '?' => Some(Condition::Unknown),
                _ => None,
            })
            .collect()
    }

    fn parse_groups(symbols: &str) -> Vec<u8> {
        symbols
            .split(',')
            .filter_map(|num| num.parse::<u8>().ok())
            .collect()
    }
}
