use std::collections::{HashMap, HashSet};

use module::Module;

mod module;
mod parser;

fn main() {
    let h1: HashMap<String, Module> =
        HashMap::from([("B".to_owned(), Module::Broadcaster(HashSet::new()))]);
    let h2: HashMap<String, Module> =
        HashMap::from([("B".to_owned(), Module::Broadcaster(HashSet::new()))]);
    let eq = h1 == h2;
    println!("{eq}")
}
