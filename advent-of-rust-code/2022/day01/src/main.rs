#![allow(dead_code, unused_mut, unused_variables)]
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/1.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {}", answer_1);
    println!("part 2: {}", answer_2);
}

fn solve_part_one(input: &str) -> i32 {
    let elves: Vec<&str> = input.split("\n\n").collect();
    let calories_per_elf = calories_summed_per_elf(&elves);
    *calories_per_elf.iter().max().unwrap()
}

fn solve_part_two(input: &str) -> i32 {
    let elves: Vec<&str> = input.split("\n\n").collect();
    let mut calories_per_elf = calories_summed_per_elf(&elves);
    calories_per_elf.sort();
    calories_per_elf[elves.len() - 3..elves.len()].iter().sum()
}

fn calories_summed_per_elf(elves: &[&str]) -> Vec<i32> {
    let calories_per_elf: Vec<Vec<i32>> = elves
        .iter()
        .map(|lines| lines.lines().collect::<Vec<&str>>())
        .map(|xs| xs.iter().map(|x| x.parse().unwrap()).collect::<Vec<i32>>())
        .collect();

    calories_per_elf
        .iter()
        .map(|cals| cals.iter().sum())
        .collect()
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str =
        "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n";
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 24000);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 45000);
    }
}
