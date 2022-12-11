#![allow(dead_code, unused_mut, unused_variables)]

pub use std::collections::HashMap;
use std::{collections::VecDeque, convert::Infallible, str::FromStr};

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/10.input").unwrap()
}

fn main() {
    print_time!("execution");
    // let answer_1 = solve_part_one(&input_from_file());
    solve_part_two(&input_from_file());

    // println!("part 1: {answer_1}");
}

fn solve_part_one(input: &str) -> i64 {
    let instructions = parse_input(input);
    let signal_strengths = run_instructions(&instructions);

    let s20 = signal_strengths.get(20).unwrap() * 20;
    let s60 = signal_strengths.get(60).unwrap() * 60;
    let s100 = signal_strengths.get(100).unwrap() * 100;
    let s140 = signal_strengths.get(140).unwrap() * 140;
    let s180 = signal_strengths.get(180).unwrap() * 180;
    let s220 = signal_strengths.get(220).unwrap() * 220;
    println!("{s20}, {s60}, {s100}, {s140}, {s180}, {s220}");
    [s20, s60, s100, s140, s180, s220].iter().sum()
}

fn solve_part_two(input: &str) {
    let instructions = parse_input(input);
    draw_pixels(&instructions);
}

fn run_instructions(instructions: &[Instruction]) -> Vec<i64> {
    let mut x_register: Vec<i64> = vec![0, 1];
    let mut iter = instructions.iter();
    let mut queued: VecDeque<i64> = VecDeque::new();
    let mut busy: bool = false;
    // println!("instructions {instructions:?}");
    for cycle in 1..=220 {
        let x_at_cycle_start = x_register.last().unwrap();
        println!("------------------------------------------------------");
        println!("during cycle {cycle}, X = {x_at_cycle_start}");
        if busy {
            busy = false;
            let new_x = x_at_cycle_start + queued.pop_front().unwrap();
            println!("finished calculating and after this cycle, X is now {new_x}");
            x_register.push(new_x);
        } else if let Some(instruction) = iter.next() {
            x_register.push(*x_at_cycle_start);
            println!("instruction: {instruction:?}");
            match instruction {
                Instruction::Noop => {
                    println!("doing nothing");
                }
                Instruction::Addx(x) => {
                    println!("starting calculating + {x}");
                    queued.push_back(*x);
                    busy = true;
                }
            }
        } else {
            panic!("no more instructions!")
        }
    }
    x_register
}

fn draw_pixels(instructions: &[Instruction]) {
    let mut x_register: Vec<i64> = vec![1];
    let mut iter = instructions.iter();
    let mut queued: VecDeque<i64> = VecDeque::new();
    let mut busy: bool = false;
    let mut pixel_buffer: [char; 240] = ['.'; 240];

    for cycle in 1..240 {
        let x = x_register.last().unwrap();
        let offset = match cycle {
            1..=40 => 0,
            41..=80 => 40,
            81..=120 => 80,
            121..=160 => 120,
            161..=200 => 160,
            201..=240 => 200,
            _ => panic!("cycle out of range"),
        };

        let pixel = if x == &(cycle - 1 - 1 - offset)
            || x == &(cycle - 1 - offset)
            || x == &(cycle - offset)
        {
            '⬜'
        } else {
            '⬛'
        };

        pixel_buffer[cycle as usize] = pixel;
        if busy {
            busy = false;
            let new_x = x + queued.pop_front().unwrap();

            x_register.push(new_x);
        } else if let Some(instruction) = iter.next() {
            x_register.push(*x);

            match instruction {
                Instruction::Noop => {}
                Instruction::Addx(x) => {
                    queued.push_back(*x);
                    busy = true;
                }
            }
        }
    }
    pretty_print(pixel_buffer);
}

fn pretty_print(screen: [char; 240]) {
    (0..240).for_each(|n| {
        if n % 40 == 0 {
            println!();
        } else {
            print!("{}", screen[n]);
        }
    });
    println!();
    println!();
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .flat_map(Instruction::from_str)
        .collect::<Vec<_>>()
}

#[derive(Debug)]
enum Instruction {
    Noop,
    Addx(i64),
}

impl FromStr for Instruction {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let instruction = if s.starts_with("noop") {
            Instruction::Noop
        } else if s.starts_with("addx") {
            let x = s.split_once(' ').unwrap().1.parse().unwrap();
            Instruction::Addx(x)
        } else {
            panic!()
        };
        Ok(instruction)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"noop
addx 3
addx -5
"#;

    const LARGER_EXAMPLE: &str = r#"addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
"#;

    #[test]
    fn test() {
        // assert_eq!(solve_part_one(EXAMPLE_INPUT), -1);
        assert_eq!(solve_part_one(LARGER_EXAMPLE), 13140);
    }
    #[test]
    fn test_two() {
        // assert_eq!(solve_part_one(EXAMPLE_INPUT), -1);
        solve_part_two(LARGER_EXAMPLE);
    }

    // #[test]
    // fn test_two() {
    //     // assert_eq!(solve_part_two(EXAMPLE_INPUT), 1);
    //     assert_eq!(solve_part_two(LARGER_EXAMPLE), 36);
    // }
}
