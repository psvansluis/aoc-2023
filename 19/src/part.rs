use std::collections::HashMap;

use regex::Regex;

use crate::workflow::{Destination, Workflow};

#[derive(Debug)]
pub struct Part {
    pub x: u32,
    pub m: u32,
    pub a: u32,
    pub s: u32,
}

impl Part {
    pub fn parse(input: &str) -> Self {
        let re = Regex::new(r"\d+").unwrap();
        let ds: Vec<u32> = re
            .find_iter(input)
            .map(|ds| ds.as_str().parse::<u32>().unwrap())
            .collect();
        Self {
            x: ds[0],
            m: ds[1],
            a: ds[2],
            s: ds[3],
        }
    }

    pub fn sum_of_ratings(&self) -> u32 {
        self.x + self.m + self.a + self.s
    }

    pub fn is_accepted(&self, hashmap: &HashMap<String, Workflow>) -> bool {
        let start = hashmap.get("in").unwrap();
        let destination = self.apply_workflow(hashmap, start);
        matches!(destination, Destination::Accepted)
    }

    fn apply_workflow<'a>(
        &'a self,
        hashmap: &'a HashMap<String, Workflow>,
        workflow: &'a Workflow,
    ) -> &Destination {
        let dest = workflow.apply(self);
        match dest {
            Destination::NextFlow(flow) => self.apply_workflow(hashmap, hashmap.get(flow).unwrap()),
            _ => dest,
        }
    }
}
