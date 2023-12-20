use std::{cmp::max, collections::HashMap};

use crate::workflow::{Destination, Workflow};

#[derive(Debug, Clone, Copy)]
pub struct FieldRange {
    pub start: u64,
    pub end: u64,
}

impl FieldRange {
    fn n_possibilities(&self) -> u64 {
        (self.end + 1).saturating_sub(self.start)
    }
}
#[derive(Debug, Clone, Copy)]
pub struct PartRange {
    pub x: FieldRange,
    pub m: FieldRange,
    pub a: FieldRange,
    pub s: FieldRange,
}

impl PartRange {
    pub fn resolve<'a>(
        &'a self,
        hashmap: &'a HashMap<String, Workflow>,
        workflow: &'a Workflow,
    ) -> Vec<(PartRange, &Destination)> {
        let mut out = workflow.apply_range(self);

        while out.iter().any(has_next) {
            // get first element with next flow
            let pos = out.iter().position(has_next).unwrap();
            let (next_range, Destination::NextFlow(map_key)): (PartRange, &Destination) = out.remove(pos) else {unreachable!()};
            let next_flow = hashmap.get(map_key).unwrap();

            // get all resolutions of this element
            let mut resolutions = next_flow.apply_range(&next_range);

            // add those resolutions to out
            out.append(&mut resolutions);
        }

        out
    }

    pub fn n_possibilities(&self) -> u64 {
        [self.x, self.m, self.a, self.s]
            .iter()
            .map(FieldRange::n_possibilities)
            .product()
    }
}

fn has_next((_range, dest): &(PartRange, &Destination)) -> bool {
    matches!(dest, Destination::NextFlow(_))
}
