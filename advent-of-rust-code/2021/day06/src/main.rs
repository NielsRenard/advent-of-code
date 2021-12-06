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
    std::fs::read_to_string("../../data/2021/6.input").unwrap()
}

fn main() {
    print_time!("execution");
    // let answer_1 = solve_brute_force(&input_from_file());
    // execution took 2.4ms
    let answer_1 = solve(&input_from_file(), 80);
    println!("part 1: {}", answer_1);
    let answer_2 = solve(&input_from_file(), 256);
    println!("part 2: {}", answer_2);
    // execution took 0.05ms
}

/*
brute force solution
*/

fn solve_brute_force(input: &str) -> i64 {
    let mut fishes: Vec<u8> = input
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(|c| c.parse().unwrap())
        .collect();
    for n in 1..81 {
        advance_one_step(&mut fishes);
        // println!("fishes after {} days: {:?}", n, fishes);
    }
    fishes.len() as i64
}

fn advance_one_step(mut fishes: &mut Vec<u8>) {
    let mut new_fishes = Vec::<u8>::new();
    fishes.iter_mut().for_each(|n| {
        if *n > 0 {
            *n -= 1
        } else {
            *n = 6;
            new_fishes.push(8);
        }
    });
    fishes.append(&mut new_fishes)
}

/*
proper solution
*/
fn solve(input: &str, days: usize) -> i64 {
    let fishes: Vec<u8> = input
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(|c| c.parse().unwrap())
        .collect();
    let mut grouped_fishes = initial_fish_groups(&fishes);
    for n in 0..days {
        advance_one_step_part_two(&mut grouped_fishes);
    }
    grouped_fishes.iter().sum()
}

fn advance_one_step_part_two(mut grouped_fishes: &mut Vec<i64>) {
    let temp_zeroes = grouped_fishes[0];
    grouped_fishes[0] = grouped_fishes[1];
    grouped_fishes[1] = grouped_fishes[2];
    grouped_fishes[2] = grouped_fishes[3];
    grouped_fishes[3] = grouped_fishes[4];
    grouped_fishes[4] = grouped_fishes[5];
    grouped_fishes[5] = grouped_fishes[6];
    grouped_fishes[6] = grouped_fishes[7];
    grouped_fishes[7] = grouped_fishes[8];
    grouped_fishes[6] += temp_zeroes;
    grouped_fishes[8] = temp_zeroes;
}

fn initial_fish_groups(fishes: &[u8]) -> Vec<i64> {
    let zeroes = fishes.iter().filter(|fish| fish == &&0).count().try_into().unwrap();
    let ones   = fishes.iter().filter(|fish| fish == &&1).count().try_into().unwrap();
    let twos   = fishes.iter().filter(|fish| fish == &&2).count().try_into().unwrap();
    let threes = fishes.iter().filter(|fish| fish == &&3).count().try_into().unwrap();
    let fours  = fishes.iter().filter(|fish| fish == &&4).count().try_into().unwrap();
    let fives  = fishes.iter().filter(|fish| fish == &&5).count().try_into().unwrap();
    let sixes  = fishes.iter().filter(|fish| fish == &&6).count().try_into().unwrap();
    let sevens = fishes.iter().filter(|fish| fish == &&7).count().try_into().unwrap();
    let eights = fishes.iter().filter(|fish| fish == &&8).count().try_into().unwrap();
    [zeroes, ones, twos, threes, fours, fives, sixes, sevens, eights].into()
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "3,4,3,1,2";
    #[test]
    fn test_1_brute_force() {
        assert_eq!(solve_brute_force(EXAMPLE_INPUT), 5934);
    }
    fn test_1() {
        assert_eq!(solve(EXAMPLE_INPUT, 80), 5934);
    }
    #[test]
    fn test_2() {
        assert_eq!(solve(EXAMPLE_INPUT, 256), 26984457539);
    }
}
