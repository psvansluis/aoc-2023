use crate::part::Part;

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
enum Category {
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
}
