#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment)]

use core::panic;
use std::{
    char::from_digit,
    collections::{hash_map, HashMap, HashSet},
    hash::Hash,
    ops::{Add, Mul, RangeBounds, Sub},
};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/10.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    // execution took 0.17ms
}

fn solve_part_one(input: &str) -> i64 {
    let mut opener_stack: Vec<char> = vec![];

    let lines = input.lines();
    let mut score: i64 = 0;
    for line in lines {
        let mut chars = line.chars().peekable();
        let mut keep_going = true;
        let mut first_error: Option<char> = None;
        while keep_going {
            match chars.next() {
                Some(opener) if ['{', '[', '<', '('].contains(&opener) => opener_stack.push(opener),
                Some(closer) => {
                    // print!("match...");
                    let opener = opener_stack.pop().unwrap();
                    if opener.eq(&closer_to_opener(&closer)) {
                    } else {
                        // println!("problem: {} {}", opener, closer);
                        keep_going = false;
                        first_error = Some(closer);
                    }
                }
                otherwise => keep_going = false,
            }
        }
        match first_error {
            Some(')') => score += 3,
            Some(']') => score += 57,
            Some('}') => score += 1197,
            Some('>') => score += 25137,
            _ => {}
        }
    }
    score
}

fn solve_part_two(input: &str) -> i64 {
    let mut opener_stack: Vec<char> = vec![];

    let lines = input.lines();
    let mut scores: Vec<i64> = vec![];
    let mut incomplete_lines: Vec<&str> = vec![];
    for line in lines {
        let mut chars = line.chars().peekable();
        let mut keep_going = true;
        let mut first_error: Option<char> = None;
        while keep_going {
            match chars.next() {
                Some(opener) if ['{', '[', '<', '('].contains(&opener) => opener_stack.push(opener),
                Some(closer) => {
                    let opener = opener_stack.pop().unwrap();
                    if opener.eq(&closer_to_opener(&closer)) {
                    } else {
                        // println!("problem: {} {}", opener, closer);
                        opener_stack.clear();
                        keep_going = false;
                        first_error = Some(closer);
                    }
                }
                otherwise => keep_going = false,
            }
        }
        if first_error.is_none() {
            let completion: Vec<char> = opener_stack.iter().rev().map(opener_to_closer).collect();
            let score = completion.iter().fold(0, |sum, c| {
                sum * 5
                    + match c {
                        ')' => 1,
                        ']' => 2,
                        '}' => 3,
                        '>' => 4,
                        _ => {
                            panic!()
                        }
                    }
            });
            scores.push(score);
            opener_stack.clear();
        }
    }
    scores.sort_unstable();
    let middle_of_list = (scores.len() / 2) + 1;
    *scores.get(middle_of_list - 1).unwrap()
}

fn closer_to_opener(closer: &char) -> char {
    match closer {
        '}' => '{',
        ']' => '[',
        '>' => '<',
        ')' => '(',
        _ => panic!(),
    }
}

fn opener_to_closer(opener: &char) -> char {
    match opener {
        '{' => '}',
        '[' => ']',
        '<' => '>',
        '(' => ')',
        _ => panic!(),
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 26397);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 288957);
    }
}
