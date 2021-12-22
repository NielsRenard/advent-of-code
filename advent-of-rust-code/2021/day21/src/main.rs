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
    let answer_2 = solve_part_two(&input);
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

fn solve_part_two(input: &str) -> u64{
    let (p1_start, p2_start) = parse(&input);
    dirac_dice_recur(1, 0, 0, p1_start as usize, p2_start as usize, 0, 0, 0)
}

fn dirac_dice_recur(dice: usize, whos_turn: usize, throws_so_far: usize, p1_pos: usize, p2_pos: usize, accum_p1_wins: u64, accum_score_p1: u64, accum_score_p2: u64) -> u64 {
    // println!("p1 pos: {:?}\np2 pos: {:?}", &p1_pos, &p2_pos);
    // println!("p1 acc score: {:?}\np2 acc score: {:?}", &accum_score_p1, &accum_score_p2);
    // println!();

     // print!("deeper");

    //     444356092776315
    // 9223372036854775807
    let mut scores = [accum_score_p1, accum_score_p2];
    let mut positions = [(1..=10).cycle().peekable(), (1..=10).cycle().peekable()];
    let _1 = positions[0].advance_by((p1_pos - 1) as usize);
    let _2 = positions[1].advance_by((p2_pos - 1) as usize);
    let mut whos_turn = whos_turn;
    let mut dimensions_where_player_one_wins = accum_p1_wins;
    let mut throws = throws_so_far;
    loop {
        println!("p{:?} turn", whos_turn+1);
        if scores.iter().any(|score| score >= &21) {
            // println!("p1 score: {:?}\np2 score: {:?}", scores[0], scores[1]);
            break;
        };
        loop {
            let _1 = positions[whos_turn].advance_by(dice);
            scores[whos_turn] += positions[whos_turn].peek().unwrap();

            if throws > 3 {
                whos_turn = if whos_turn == 0 { 1 } else { 0 };
                break;
            };
            throws += 1;

            dimensions_where_player_one_wins += dirac_dice_recur(1, whos_turn, throws, *positions[0].peek().unwrap() as usize, *positions[1].peek().unwrap() as usize, dimensions_where_player_one_wins, scores[0], scores[1]);
            dimensions_where_player_one_wins += dirac_dice_recur(2, whos_turn, throws, *positions[0].peek().unwrap() as usize, *positions[1].peek().unwrap() as usize, dimensions_where_player_one_wins, scores[0], scores[1]);
            dimensions_where_player_one_wins += dirac_dice_recur(3, whos_turn, throws, *positions[0].peek().unwrap() as usize, *positions[1].peek().unwrap() as usize, dimensions_where_player_one_wins, scores[0], scores[1]);

        }
    }

    if scores[0] > scores[1] {
        println!("p1 wins!  acc: {:?}", dimensions_where_player_one_wins);
        1
    } else {
        0
    } 
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
    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 444356092776315);
    }
}
