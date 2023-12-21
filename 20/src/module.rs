use std::collections::{HashMap, HashSet};

#[derive(PartialEq)]
pub enum Module {
    Broadcaster(HashSet<String>),
    FlipFlop(HashSet<String>, FlipFlopState),
    Conjunction(HashSet<String>, ConjunctionReceived),
}

#[derive(PartialEq)]
pub enum FlipFlopState {
    On,
    Off,
}

#[derive(PartialEq)]
pub enum ConjunctionReceived {
    None,
    High,
    LowOnly,
}

pub enum Pulse {
    Low,
    High,
}

impl Module {
    pub fn receive_pulse(&mut self, pulse: &Pulse, hashmap: &mut HashMap<String, Module>) {
        match (self, pulse) {
            (Module::Broadcaster(_), pulse) => self.targets(hashmap),
            (Module::FlipFlop(_, _), Pulse::Low) => todo!(),
            (Module::FlipFlop(_, _), Pulse::High) => todo!(),
            (Module::Conjunction(_, _), Pulse::Low) => todo!(),
            (Module::Conjunction(_, _), Pulse::High) => todo!(),
        }
    }

    fn targets<'a>(&'a self, hashmap: &'a HashMap<String, Module>) -> Vec<&Module> {
        let set = match self {
            Module::Broadcaster(set) => set,
            Module::FlipFlop(set, _) => set,
            Module::Conjunction(set, _) => set,
        };
        set.iter()
            .map(|target| hashmap.get(target).unwrap())
            .collect()
    }
}
