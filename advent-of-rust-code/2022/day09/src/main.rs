#![allow(dead_code, unused_mut, unused_variables)]

pub use std::collections::HashMap;
use std::{collections::HashSet, convert::Infallible, str::FromStr, time, thread};

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/9.input").unwrap()
}

// TODO: Make the step simulator generic so I don't need to have 2 hardcoded implementations

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());

    println!("part 1: {answer_1}");
    println!("part 2: {answer_2}");
}

fn solve_part_one(input: &str) -> usize {
    let steps = parse_input(input);
    let positions = simulate_steps(&steps);
    let unique_tail_positions = positions
        .iter()
        .map(|[_, tail_pos]| tail_pos)
        .collect::<HashSet<&[i32; 2]>>();
    unique_tail_positions.len()
}

fn solve_part_two(input: &str) -> usize {
    let steps = parse_input(input);
    let positions = simulate_steps_two(&steps);

    let unique_tail_positions = positions
        .iter()
        .map(|[_, _, _, _, _, _, _, _, _, tail_pos]| tail_pos)
        .collect::<HashSet<&[i32; 2]>>();
    unique_tail_positions.len()
}

fn simulate_steps_two(instructions: &[Instruction]) -> Vec<[[i32; 2]; 10]> {
    // e.g. [[[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0],[0,0]], ...]
    let mut positions: Vec<[[i32; 2]; 10]> = vec![[[0, 0]; 10]];
    for instruction in instructions {
        for _ in 1..=instruction.steps {
            // moving the head
            let [cur_head_x, cur_head_y] = positions.last().unwrap()[0];
            let [nxt_head_x, nxt_head_y] = match instruction.dir {
                Direction::L => [cur_head_x - 1, cur_head_y],
                Direction::R => [cur_head_x + 1, cur_head_y],
                Direction::U => [cur_head_x, cur_head_y + 1],
                Direction::D => [cur_head_x, cur_head_y - 1],
            };
            let mut last_position: [[i32; 2]; 10] = *positions.last().unwrap();
            last_position[0] = [nxt_head_x, nxt_head_y];
            positions.push(last_position);

            // dragging the tail
            // let mut new_positions: [[i32; 2]; 10] = [[0, 0]; 10];
            // new_positions[0] = [nxt_head_x, nxt_head_y];
            (1..10).for_each(|n| {
                let [cur_knot_x, cur_knot_y] = positions.last().unwrap()[n];
                let [head_x, head_y] = positions.last().unwrap()[n - 1];
                let diff = [head_x - cur_knot_x, head_y - cur_knot_y];
                let [nxt_tail_x, nxt_tail_y] = match diff {
                    // orthogonal drag                                ..a..
                    [0, 2] => [cur_knot_x, cur_knot_y + 1], //   a    .....
                    [2, 0] => [cur_knot_x + 1, cur_knot_y], //   b    d.T.b
                    [0, -2] => [cur_knot_x, cur_knot_y - 1], //  c    .....
                    [-2, 0] => [cur_knot_x - 1, cur_knot_y], //  d    ..c..
                    // diagonal drag                                                               jk.ab
                    [1, 2] | [2, 1] | [2, 2] => [cur_knot_x + 1, cur_knot_y + 1], //       a|b|c   i...c
                    [2, -1] | [1, -2] | [2, -2] => [cur_knot_x + 1, cur_knot_y - 1], //    d|e|f   ..T..
                    [-1, -2] | [-2, -1] | [-2, -2] => [cur_knot_x - 1, cur_knot_y - 1], // g|h|i   i...d
                    [-2, 1] | [-1, 2] | [-2, 2] => [cur_knot_x - 1, cur_knot_y + 1], //    j|k|l   hg.fe
                    // tail doesn't move
                    _ => [cur_knot_x, cur_knot_y],
                };
                positions.last_mut().unwrap()[n] = [nxt_tail_x, nxt_tail_y];
            });
            pretty_print_two(positions.last().unwrap());
            println!();
        }
    }
    positions
}

fn simulate_steps(instructions: &[Instruction]) -> Vec<[[i32; 2]; 2]> {
    let start_head = [0, 0];
    let start_tail = [0, 0];
    // e.g. [[[0,0],[0,0]], [[1,0],[0,0]],[[2,0],[1,0]]]
    let mut positions: Vec<[[i32; 2]; 2]> = vec![[start_head, start_tail]];
    for instruction in instructions {
        for n in 1..=instruction.steps {
            let [cur_head_x, cur_head_y] = positions.last().unwrap()[0];
            let [nxt_head_x, nxt_head_y] = match instruction.dir {
                Direction::L => [cur_head_x - 1, cur_head_y],
                Direction::R => [cur_head_x + 1, cur_head_y],
                Direction::U => [cur_head_x, cur_head_y + 1],
                Direction::D => [cur_head_x, cur_head_y - 1],
            };
            let [cur_tail_x, cur_tail_y] = positions.last().unwrap()[1];
            let diff = [nxt_head_x - cur_tail_x, nxt_head_y - cur_tail_y];
            let [nxt_tail_x, nxt_tail_y] = match diff {
                // orthogonal drag                                         a
                [0, 2] => [cur_tail_x, cur_tail_y + 1], //          a      .
                [2, 0] => [cur_tail_x + 1, cur_tail_y], //          b    d.T.b
                [0, -2] => [cur_tail_x, cur_tail_y - 1], //         c      .
                [-2, 0] => [cur_tail_x - 1, cur_tail_y], //         d      c
                // diagonal drag                                                     h a
                [1, 2] | [2, 1] => [cur_tail_x + 1, cur_tail_y + 1], //     a|b     g. .b
                [2, -1] | [1, -2] => [cur_tail_x + 1, cur_tail_y - 1], //   c|d       T
                [-1, -2] | [-2, -1] => [cur_tail_x - 1, cur_tail_y - 1], // e|f     f. .c
                [-2, 1] | [-1, 2] => [cur_tail_x - 1, cur_tail_y + 1], //   g|h      e d
                // tail doesn't move
                _ => [cur_tail_x, cur_tail_y],
            };
            positions.push([[nxt_head_x, nxt_head_y], [nxt_tail_x, nxt_tail_y]]);
            // pretty_print(positions.last().unwrap());
        }
    }
    positions
}

/// Visualizes the current position of the rope
/// Screen size is constrained to small example input data
fn pretty_print(positions: &[[i32; 2]; 2]) {
    for y in 0..=6 {
        for x in 0..=6 {
            if positions[0] == [x, y] {
                print!("H")
            } else if positions[1] == [x, y] {
                print!("T")
            } else {
                print!(".")
            }
        }
        println!();
    }
    println!();
}

/// Visualizes the current position of the rope
fn pretty_print_two(positions: &[[i32; 2]; 10]) {
    let ten_millis = time::Duration::from_millis(100);
    let now = time::Instant::now();
    thread::sleep(ten_millis);
    
    for y in (-10..=70).rev() {
        for x in -30..=30 {
            if positions[0] == [x, y] {
                print!("H")
            } else if positions[1] == [x, y] {
                print!("1")
            } else if positions[2] == [x, y] {
                print!("2")
            } else if positions[3] == [x, y] {
                print!("3")
            } else if positions[4] == [x, y] {
                print!("4")
            } else if positions[5] == [x, y] {
                print!("5")
            } else if positions[6] == [x, y] {
                print!("6")
            } else if positions[7] == [x, y] {
                print!("7")
            } else if positions[8] == [x, y] {
                print!("8")
            } else if positions[9] == [x, y] {
                print!("9")
            } else {
                print!(".")
            }
        }
        println!();
    }
    println!();
}

fn parse_input(input: &str) -> Vec<Instruction> {
    input.lines().flat_map(Instruction::from_str).collect()
}

#[derive(Debug)]
struct Instruction {
    dir: Direction,
    steps: usize,
}

#[derive(Debug)]
pub enum Direction {
    L,
    R,
    U,
    D,
}

impl FromStr for Instruction {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (direction, count) = s.split_once(' ').unwrap();
        let step = match (direction, count.parse().unwrap()) {
            ("L", n) => Instruction {
                dir: Direction::L,
                steps: n,
            },
            ("R", n) => Instruction {
                dir: Direction::R,
                steps: n,
            },
            ("U", n) => Instruction {
                dir: Direction::U,
                steps: n,
            },
            ("D", n) => Instruction {
                dir: Direction::D,
                steps: n,
            },
            _ => panic!("invalid direction"),
        };
        Ok(step)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
"#;

    const LARGER_EXAMPLE: &str = r#"R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
"#;

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 13);
    }
    #[test]
    fn test_two() {
        // assert_eq!(solve_part_two(EXAMPLE_INPUT), 1);
        assert_eq!(solve_part_two(LARGER_EXAMPLE), 36);
    }
}
