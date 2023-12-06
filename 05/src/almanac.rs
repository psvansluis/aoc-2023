use regex::Regex;
use std::collections::HashSet;

#[derive(Debug, PartialEq)]
struct Mapping(HashSet<(u64, u64, u64)>);

impl Mapping {
    fn new(set: HashSet<(u64, u64, u64)>) -> Self {
        Mapping(set)
    }

    fn get(&self, key: u64) -> u64 {
        self.0
            .iter()
            .find_map(|(value, key_inner, range)| {
                if key >= *key_inner && key < key_inner + range {
                    Some(key - key_inner + value)
                } else {
                    None
                }
            })
            .unwrap_or(key)
    }
}

#[derive(Debug)]
pub struct Almanac {
    seeds: HashSet<u64>,
    mappings: Vec<Mapping>,
}

impl Almanac {
    pub fn new(input: &str) -> Option<Self> {
        let seeds = Self::parse_seeds(input)?;
        let mappings = Self::parse_mappings(input)?;
        Some(Self { seeds, mappings })
    }

    fn parse_seeds(input: &str) -> Option<HashSet<u64>> {
        Some(
            Regex::new(r"(?m)seeds:([\s\d]+)\n")
                .ok()?
                .captures(input)?
                .get(1)?
                .as_str()
                .split_ascii_whitespace()
                .map(|str| str.parse::<u64>().unwrap())
                .collect::<HashSet<u64>>(),
        )
    }

    fn parse_mapping(input: &str, name: &str) -> Option<Mapping> {
        Some(Mapping::new(
            Regex::new(&format!(r"(?m){name} map:\r\n([\s\d]+)"))
                .ok()?
                .captures(input)?
                .get(1)?
                .as_str()
                .lines()
                .flat_map(|line| {
                    match line
                        .split_ascii_whitespace()
                        .map(|str| str.parse::<u64>().unwrap())
                        .collect::<Vec<u64>>()[..]
                    {
                        [fst, snd, thrd] => Some((fst, snd, thrd)),
                        _ => None,
                    }
                })
                .collect::<HashSet<(u64, u64, u64)>>(),
        ))
    }

    fn parse_mappings(input: &str) -> Option<Vec<Mapping>> {
        [
            "seed-to-soil",
            "soil-to-fertilizer",
            "fertilizer-to-water",
            "water-to-light",
            "light-to-temperature",
            "temperature-to-humidity",
            "humidity-to-location",
        ]
        .iter()
        .map(|name| Self::parse_mapping(input, name))
        .collect()
    }

    fn seed_location(&self, seed: u64) -> u64 {
        self.mappings
            .iter()
            .fold(seed, |acc, mapping| mapping.get(acc))
    }

    pub fn seed_locations(&self) -> Vec<u64> {
        self.seeds
            .iter()
            .map(|seed| self.seed_location(*seed))
            .collect()
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashSet, fs};

    use crate::almanac::{self, Almanac, Mapping};

    fn example() -> String {
        fs::read_to_string("./resources/example.txt").expect("to read file")
    }

    #[test]
    fn test_parse_seeds() {
        let expected: HashSet<u64> = vec![79, 14, 55, 13].into_iter().collect();
        assert_eq!(Some(expected), Almanac::parse_seeds(&example()));
    }

    #[test]
    fn test_parse_mapping_1() {
        let expected: Mapping = almanac::Mapping(HashSet::from([(50, 98, 2), (52, 50, 48)]));
        let actual = Almanac::parse_mapping(&example(), "seed-to-soil");
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn test_parse_mapping_2() {
        let expected: Mapping =
            almanac::Mapping(HashSet::from([(0, 15, 37), (37, 52, 2), (39, 0, 15)]));
        let actual = Almanac::parse_mapping(&example(), "soil-to-fertilizer");
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn test_hashmap_value_mapped() {
        let mapping: Mapping = almanac::Mapping(HashSet::from([(50, 98, 2), (52, 50, 48)]));
        let input = 79;
        let expected = 81;
        assert_eq!(mapping.get(input), expected);
    }
    #[test]
    fn test_hashmap_value_unmapped() {
        let mapping: Mapping = almanac::Mapping(HashSet::from([(50, 98, 2), (52, 50, 48)]));
        let input = 13;
        let expected = 13;
        assert_eq!(mapping.get(input), expected);
    }
}
