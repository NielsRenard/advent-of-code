#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by)]

use std::ops::{Add, Mul, Sub};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/4.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1); // execution took 0.6ms
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    // execution took 3.57ms
}

type Board = Vec<Vec<u8>>;
type Column = Vec<u8>;

fn columns(board: &Board) -> Vec<Column> {
    let mut columns: Board = vec![];
    for n in 0..board[0].len() {
        columns.push(board.iter().map(|row| row[n]).collect())
    }
    columns
}

fn rows(board: &Board) -> &Board {
    board
}

fn solve_part_one(input: &str) -> u32 {
    let (mut random_draws, boards): (Vec<u8>, Vec<Board>) = parse_input(input);

    let mut numbers_drawn_so_far: Vec<u8> = random_draws[0..5].to_vec();
    let mut random_draws_iter = random_draws.iter().peekable();
    random_draws_iter.advance_by(5).unwrap();

    let mut bingo = false;
    let mut answer = 0;
    while !bingo {
        numbers_drawn_so_far.push(*random_draws_iter.next().unwrap());
        for board in &boards {
            board.iter().for_each(|row| {
                let all_present = row.iter().all(|num| numbers_drawn_so_far.contains(num));
                if all_present {
                    bingo = true
                };
            });

            // tiny optimization
            if !bingo {
                columns(board).iter().for_each(|column| {
                    let all_present = column.iter().all(|num| numbers_drawn_so_far.contains(num));
                    if all_present {
                        bingo = true;
                    };
                });
            }

            if bingo {
                answer = board
                    .concat()
                    .iter()
                    .filter(|n| !numbers_drawn_so_far.contains(n))
                    .map(|n| *n as u32)
                    .sum::<u32>()
                    .mul(*numbers_drawn_so_far.last().unwrap() as u32);
                break; // bingo was set to true
            }
        }
    }
    answer
}

fn solve_part_two(input: &str) -> u32 {
    let (mut random_draws, mut boards): (Vec<u8>, Vec<Board>) = parse_input(input);
    let mut numbers_drawn_so_far: Vec<u8> = random_draws[0..5].to_vec();
    let mut random_draws_iter = random_draws.iter();
    random_draws_iter.advance_by(5).unwrap();

    let mut bingo_boards: Vec<&Board> = vec![];
    while bingo_boards.len() < boards.len() {
        numbers_drawn_so_far.push(*random_draws_iter.next().unwrap());
        let non_bingo_boards: Vec<&Board> = boards
            .iter()
            .filter(|b| !bingo_boards.contains(b))
            .collect();
        non_bingo_boards.into_iter().for_each(|board| {
            let mut bingo = false;
            board.iter().for_each(|row| {
                let all_present = row.iter().all(|num| numbers_drawn_so_far.contains(num));
                if all_present {
                    bingo = true;
                };
            });
            // tiny optimization
            if !bingo {
                columns(board).iter().for_each(|column| {
                    let all_present = column.iter().all(|num| numbers_drawn_so_far.contains(num));
                    if all_present {
                        bingo = true;
                    };
                });
            }

            if bingo && !bingo_boards.contains(&board) {
                bingo_boards.push(board);
            }
        });
    }
    let answer: u32 = bingo_boards
        .last()
        .unwrap()
        .concat()
        .iter()
        .filter(|n| !numbers_drawn_so_far.contains(n))
        .map(|n| *n as u32)
        .sum::<u32>()
        .mul(*numbers_drawn_so_far.last().unwrap() as u32);
    answer
}

fn parse_input(input: &str) -> (Vec<u8>, Vec<Board>) {
    let mut lines = input.lines().peekable();
    let drawn_numbers: Vec<u8> = lines
        .next()
        .unwrap()
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();

    let mut boards: Vec<Board> = vec![];
    while lines.peek().is_some() {
        let _separator = lines.next().unwrap();
        let five_lines = lines.clone().take(5); // figure out how to not do this clone, to maybe get sub 1ms again
        let board: Board = five_lines
            .map(|s| s.split_whitespace().map(|n| n.parse().unwrap()).collect())
            .collect();
        let _move = lines.advance_by(5);
        boards.push(board);
    }
    (drawn_numbers, boards)
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str =
        "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7";
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 4512);
    }
    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 1924);
    }
}
