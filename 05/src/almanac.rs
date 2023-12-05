use regex::Regex;
use std::{
    collections::{HashMap, HashSet},
    iter::zip,
};

type Mapping = HashSet<(u32, u32, u32)>;

#[derive(Debug)]
pub struct Almanac {
    seeds: HashSet<u32>,
    seed_to_soil: HashMap<u32, u32>,
    soil_to_fertilizer: HashMap<u32, u32>,
    fertilizer_to_water: HashMap<u32, u32>,
    water_to_light: HashMap<u32, u32>,
    light_to_temperature: HashMap<u32, u32>,
    temperature_to_humidity: HashMap<u32, u32>,
    humidity_to_location: HashMap<u32, u32>,
}

impl Almanac {
    pub fn new(input: &str) -> Option<Self> {
        let seeds = Self::parse_seeds(input)?;
        let seed_to_soil = Self::parse_to_hashmap(input, "seed-to-soil")?;
        let soil_to_fertilizer = Self::parse_to_hashmap(input, "soil-to-fertilizer")?;
        let fertilizer_to_water = Self::parse_to_hashmap(input, "fertilizer-to-water")?;
        let water_to_light = Self::parse_to_hashmap(input, "water-to-light")?;
        let light_to_temperature = Self::parse_to_hashmap(input, "light-to-temperature")?;
        let temperature_to_humidity = Self::parse_to_hashmap(input, "temperature-to-humidity")?;
        let humidity_to_location = Self::parse_to_hashmap(input, "humidity-to-location")?;
        Some(Self {
            seeds,
            seed_to_soil,
            soil_to_fertilizer,
            fertilizer_to_water,
            water_to_light,
            light_to_temperature,
            temperature_to_humidity,
            humidity_to_location,
        })
    }

    fn parse_seeds(input: &str) -> Option<HashSet<u32>> {
        Some(
            Regex::new(r"(?m)seeds:([\s\d]+)\n")
                .ok()?
                .captures(input)?
                .get(1)?
                .as_str()
                .split_ascii_whitespace()
                .map(|str| str.parse::<u32>().unwrap())
                .collect::<HashSet<u32>>(),
        )
    }

    fn parse_mapping(input: &str, name: &str) -> Option<Mapping> {
        Some(
            Regex::new(&format!(r"(?m){name} map:\r\n([\s\d]+)"))
                .ok()?
                .captures(input)?
                .get(1)?
                .as_str()
                .lines()
                .flat_map(|line| {
                    match line
                        .split_ascii_whitespace()
                        .map(|str| str.parse::<u32>().unwrap())
                        .collect::<Vec<u32>>()[..]
                    {
                        [fst, snd, thrd] => Some((fst, snd, thrd)),
                        _ => None,
                    }
                })
                .collect(),
        )
    }

    fn mapping_to_hashmap(set: Mapping) -> HashMap<u32, u32> {
        set.iter().fold(HashMap::new(), |mut acc, row| {
            acc.extend(Self::mapping_row_to_hashmap(*row));
            acc
        })
    }

    fn mapping_row_to_hashmap(row: (u32, u32, u32)) -> HashMap<u32, u32> {
        let keys = row.1..;
        let values = row.0..;
        HashMap::from_iter(zip(keys, values).take(row.2 as usize))
    }

    fn parse_to_hashmap(input: &str, name: &str) -> Option<HashMap<u32, u32>> {
        Some(Self::mapping_to_hashmap(Self::parse_mapping(input, name)?))
    }

    fn hashmap_value(key: &u32, hashmap: &HashMap<u32, u32>) -> u32 {
        *hashmap.get(key).unwrap_or(key)
    }

    fn seed_location(&self, seed: u32) -> u32 {
        vec![
            &self.seed_to_soil,
            &self.soil_to_fertilizer,
            &self.fertilizer_to_water,
            &self.water_to_light,
            &self.light_to_temperature,
            &self.temperature_to_humidity,
            &self.humidity_to_location,
        ]
        .into_iter()
        .fold(seed, |acc, el| Self::hashmap_value(&acc, el))
    }

    pub fn seed_locations(&self) -> Vec<u32> {
        self.seeds
            .iter()
            .map(|seed| self.seed_location(*seed))
            .collect()
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashSet, fs};

    use crate::almanac::{Almanac, Mapping};

    fn example() -> String {
        fs::read_to_string("./resources/example.txt").expect("to read file")
    }

    fn almanac() -> Almanac {
        Almanac::new(&example()).unwrap()
    }
    #[test]
    fn test_parse_seeds() {
        let expected: HashSet<u32> = vec![79, 14, 55, 13].into_iter().collect();
        assert_eq!(Some(expected), Almanac::parse_seeds(&example()));
    }

    #[test]
    fn test_parse_mapping_1() {
        let expected: Mapping = HashSet::from([(50, 98, 2), (52, 50, 48)]);
        let actual = Almanac::parse_mapping(&example(), "seed-to-soil");
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn test_parse_mapping_2() {
        let expected: Mapping = HashSet::from([(0, 15, 37), (37, 52, 2), (39, 0, 15)]);
        let actual = Almanac::parse_mapping(&example(), "soil-to-fertilizer");
        assert_eq!(Some(expected), actual);
    }

    #[test]
    fn test_hashmap_value_mapped() {
        let almanac = almanac();
        let input = 79;
        let expected = 81;
        assert_eq!(
            Almanac::hashmap_value(&input, &almanac.seed_to_soil),
            expected
        );
    }
    #[test]
    fn test_hashmap_value_unmapped() {
        let almanac = almanac();
        let input = 13;
        let expected = 13;
        assert_eq!(
            Almanac::hashmap_value(&input, &almanac.seed_to_soil),
            expected
        );
    }
}
