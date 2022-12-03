#![allow(dead_code, unused_mut, unused_variables)]
#![feature(iter_array_chunks)]

use std::collections::HashSet;
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/3.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {answer_1}");
    println!("part 2: {answer_2}");
}

fn solve_part_one(input: &str) -> u32 {
    input
        .lines()
        .map(|l| {
            let (one, two) = l.split_at(l.len() / 2);
            let set_one = one.chars().collect::<HashSet<char>>();
            let set_two = two.chars().collect::<HashSet<char>>();
            let letter = *set_one.intersection(&set_two).collect::<Vec<_>>()[0];
            item_to_priority(&letter)
        })
        .sum()
}

fn item_to_priority(item: &char) -> u32 {
    if item.is_lowercase() {
        *item as u32 - 96
    } else {
        *item as u32 - 38
    }
}

fn solve_part_two(input: &str) -> u32 {
    let mut sum: u32 = 0;
    for [x, y, z] in input.lines().array_chunks() {
        let set_one = x.chars().collect::<HashSet<char>>();
        let set_two = y.chars().collect::<HashSet<char>>();
        let set_three = z.chars().collect::<HashSet<char>>();

        let common_item: &char = set_one
            .iter()
            .filter(|c| set_two.contains(c) && set_three.contains(c))
            .collect::<Vec<_>>()[0];

        sum += item_to_priority(common_item);
    }
    sum
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
"#;

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 157);
    }

    #[test]
    fn test_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 70);
    }
}
