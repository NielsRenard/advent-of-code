#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]
use itertools::Itertools;
use std::collections::HashMap;
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/14.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file(), 10);
    println!("part 1: {}", answer_1);
    let answer_2 = solve_part_two(&input_from_file(), 40);
    println!("part 2: {}", answer_2);
    //execution took 2ms
}

fn solve_part_one(input: &str, steps: usize) -> usize {
    let (template, rules) = parse(input);

    let mut polymer = template;
    for n in 1..=steps {
        polymer = step(&polymer, &rules);
    }
    let counts: Vec<usize> = polymer
        .chars()
        .counts()
        .into_values()
        .sorted()
        .collect_vec();
    // println!("{:?}", polymer);
    counts.last().unwrap() - counts[0]
}

fn solve_part_two(input: &str, steps: usize) -> usize {
    let (template, rules) = parse(input);
    let polymer: Vec<char> = template.chars().into_iter().collect::<Vec<_>>();
    let mut initial_pairs: HashMap<String, usize> = HashMap::new();
    polymer.windows(2).for_each(|pair| {
        let key = format!("{}{}", pair[0], pair[1]);
        *initial_pairs.entry(key).or_insert(0) += 1;
    });

    // println!("initial pairs: {:?}",  initial_pairs);
    for n in 1..=steps {
        step_pairs(&mut initial_pairs, &rules);
    }

    let mut letters: HashMap<char, usize> = HashMap::new();
    let last_letter: &char = polymer.last().unwrap();
    letters.insert(*last_letter, 1);
    for (pair, count) in &initial_pairs {
        let first_letter = pair.chars().into_iter().next().unwrap();
        *letters.entry(first_letter).or_insert(0) += count;
    }

    // println!("Letters: {:?}",  letters);
    let counts = letters.into_values().sorted().collect_vec();
    counts.last().unwrap() - counts[0]
}

fn step_pairs(mut pair_counts_map: &mut HashMap<String, usize>, rules: &HashMap<String, String>) {
    let pair_counts_list = Vec::from_iter(pair_counts_map.iter().map(|(k, v)| (k.clone(), *v)));

    for (pair, count) in pair_counts_list {
        let new_letter = rules.get(&pair).unwrap();
        // this unwrap() assumes there are rules for all combinations

        let new_pair_a = format!("{}{}", pair.chars().into_iter().next().unwrap(), new_letter);
        let new_pair_b = format!("{}{}", new_letter, pair.chars().into_iter().nth(1).unwrap());

        // Subtract the corrent amount of broken connections
        // Remove entry if there are none left
        // Maybe this can be done in a smarter way
        let current_pair = pair_counts_map.get_mut(&pair).unwrap();
        if *current_pair > count {
            *current_pair -= count
        } else {
            pair_counts_map.remove(&pair);
        }
        // Insert newly created pairs
        (*pair_counts_map.entry(new_pair_a).or_insert(0)) += count;
        (*pair_counts_map.entry(new_pair_b).or_insert(0)) += count;
    }
}

fn step(polymer: &str, rules: &HashMap<String, String>) -> String {
    let mut new_polymer = String::new();
    new_polymer.push(polymer.chars().next().unwrap());
    polymer
        .chars()
        .into_iter()
        .collect::<Vec<_>>()
        .windows(2)
        .for_each(|pair: &[char]| {
            let maybe_rule = rules.get(&*pair.iter().collect::<String>());
            match maybe_rule {
                Some(letter) => {
                    new_polymer.push(letter.chars().next().unwrap());
                    new_polymer.push(pair[1]);
                }
                None => {
                    new_polymer.push(pair[1]);
                }
            }
        });
    new_polymer
}

fn parse(input: &str) -> (String, HashMap<String, String>) {
    let mut lines_iter = input.lines().into_iter().peekable();
    let template = lines_iter.next().unwrap().to_string();
    let rules = lines_iter
        .skip(1)
        .map(|line| line.split_once(" -> ").unwrap())
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect::<HashMap<String, String>>();

    (template, rules)
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT, 10), 1588);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT, 40), 2188189693529);
    }
}
