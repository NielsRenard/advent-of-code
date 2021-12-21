#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use itertools::Itertools;
use std::{collections::HashSet, num};

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/21.input").unwrap()
}

fn main() {
    print_time!("execution");
    let input = &input_from_file();
    let answer_1 = solve_part_one(&input);
    println!("part 1: {:?}", answer_1);
    // execution took: 0.16ms
}

fn solve_part_one(input: &str) -> u64 {
    let (p1_start, p2_start) = parse(&input);
    // println!("p1 start: {:?}\np2 start: {:?}", p1_start, p2_start);
    // println!();

    let mut scores = [0, 0];
    let mut positions = [(1..=10).cycle().peekable(), (1..=10).cycle().peekable()];
    let _1 = positions[0].advance_by((p1_start - 1) as usize);
    let _2 = positions[1].advance_by((p2_start - 1) as usize);
    let mut whos_turn = 0;
    let mut dice = 1;
    let mut dice_rolled: u64 = 0;
    loop {
        if scores.iter().any(|score| score >= &1000) {
            // println!("p1 score: {:?}\np2 score: {:?}", scores[0], scores[1]);
            break;
        };
        let mut throws = vec![dice];
        loop {
            let _1 = positions[whos_turn].advance_by(dice);

            if dice < 100 {
                dice += 1;
            } else {
                dice = 1;
            }
            dice_rolled += 1;
            let mut new_score = 0;
            if throws.len() == 3 {
                scores[whos_turn] += positions[whos_turn].peek().unwrap();
                // println!(
                //     "player {} rolled {}+{}+{}, and moves to space {:?}, for total score of {}.",
                //     whos_turn + 1,
                //     throws[0],
                //     throws[1],
                //     throws[2],
                //     &positions[whos_turn].peek().unwrap(),
                //     &scores[whos_turn]
                // );
                whos_turn = if whos_turn == 0 { 1 } else { 0 };
                break;
            };

            throws.push(dice);
        }
    }
    // println!("dice_rolled: {:?}", dice_rolled);
    let loser = scores.iter().min().unwrap();
    loser * dice_rolled
}

fn parse(input: &str) -> (u32, u32) {
    let start_positions: Vec<_> = input
        .lines()
        .map(|l| l.split_whitespace())
        .map(|mut words| words.nth(4).unwrap())
        .map(|num_str| num_str.parse().unwrap())
        .collect();
    (start_positions[0], start_positions[1])
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "Player 1 starting position: 4
Player 2 starting position: 8";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 739785);
    }
}
