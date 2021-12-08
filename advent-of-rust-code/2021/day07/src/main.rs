#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment)]

use std::{
    collections::{hash_map, HashMap},
    hash::Hash,
    ops::{Add, Mul, Sub},
};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/7.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    // execution took 4ms
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    // execution took 7.13ms
}

fn solve_part_one(input: &str) -> i32 {
    let crabs: Vec<i32> = input
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(|c| c.parse().unwrap())
        .collect();

    let mut all_distances = vec![];
    crabs.iter().for_each(|crab| {
        let mut distances = vec![];
        crabs.iter().for_each(|other_crab| {
            distances.push((crab - other_crab).abs());
        });
        all_distances.push(distances);
    });
    *all_distances
        .iter()
        .map(|ds| ds.iter().sum())
        .collect::<Vec<i32>>()
        .iter()
        .min()
        .unwrap()
}

fn solve_part_two(input: &str) -> i32 {
    let crabs: Vec<i32> = input
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(|c| c.parse().unwrap())
        .collect();

    let mut all_distances = vec![];
    crabs.iter().for_each(|crab| {
        let mut distances = vec![];
        for other_crab in crabs.iter() {
            let steps = (crab - other_crab).abs();
            // if steps == 0 { continue }
            // let fuel = if steps == 1 { 1 } else { (0..steps+1).sum::<i32>() };
            let fuel = (0..steps + 1).sum::<i32>();
            distances.push(fuel);
        }
        all_distances.push(distances);
    });
    *all_distances
        .iter()
        .map(|ds| ds.iter().sum())
        .collect::<Vec<i32>>()
        .iter()
        .min()
        .unwrap()
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "16,1,2,0,4,2,7,1,2,14";
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 37);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 168);
    }
}
