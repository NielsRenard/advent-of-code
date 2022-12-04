#![allow(dead_code, unused_mut, unused_variables)]

use std::ops::RangeInclusive;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/4.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {answer_1}");
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {answer_2}");
}

fn solve_part_one(input: &str) -> u32 {
    let range_pairs = extract_pair_ranges(input);

    let mut count = 0;
    for (mut range_a, mut range_b) in range_pairs {
        if range_a.start() >= range_b.start() && range_a.end() <= range_b.end()
            || range_b.start() >= range_a.start() && range_b.end() <= range_a.end()
        {
            count += 1;
        }
    }
    count
}

fn solve_part_two(input: &str) -> u32 {
    let range_pairs = extract_pair_ranges(input);

    let mut count = 0;
    for (mut range_a, mut range_b) in range_pairs {
        if range_a.any(|n| range_b.contains(&n)) || range_b.any(|n| range_a.contains(&n)) {
            count += 1;
        }
    }
    count
}

fn extract_pair_ranges(input: &str) -> Vec<(RangeInclusive<u32>, RangeInclusive<u32>)> {
    let range_pairs: Vec<_> = input
        .lines()
        .map(|l| {
            let (elf_a, elf_b) = l.split_once(',').unwrap();
            let (elf_a_from_str, elf_a_to_str) = elf_a.split_once('-').unwrap();
            let (elf_b_from_str, elf_b_to_str) = elf_b.split_once('-').unwrap();

            let range_a = elf_a_from_str.parse().unwrap()..=elf_a_to_str.parse().unwrap();
            let range_b = elf_b_from_str.parse().unwrap()..=elf_b_to_str.parse().unwrap();
            (range_a, range_b)
        })
        .collect();
    range_pairs
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
"#;

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 2);
    }
    #[test]
    fn test_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 4);
    }
}
