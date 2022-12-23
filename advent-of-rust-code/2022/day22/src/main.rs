#![feature(result_option_inspect, string_extend_from_within)]
#![allow(dead_code)]

use std::{thread, time};

use itertools::Itertools;
use log::Level;

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/22.input").unwrap()
}
const BLOCK: char = '█';

#[derive(Debug)]
enum Step {
    L,
    R,
    Steps(usize),
}
type Board = Vec<Vec<char>>;

#[doc(hidden)]
fn main() {
    env_logger::init();
    print_time!("execution");
    let answer_1: usize = solve_part_1(&input_from_file());
    println!("part 1: {answer_1}");
}

fn solve_part_1(input: &str) -> usize {
    let (mut board, starting_pos, path) = parse_input(input);
    println!("initial board:");
    pretty_print(&board, starting_pos, 0);

    let (end_pos @ (x, y), end_facing) = follow_path(&mut board, starting_pos, path);
    println!("{end_pos:?}, {end_facing}");
    (y + 1) * 1000 + (x + 1) * 4 + end_facing
}

fn follow_path(
    board: &mut Board,
    starting_pos: (usize, usize),
    path: Vec<Step>,
) -> ((usize, usize), usize) {
    let mut facing = [0, 1, 2, 3]; // RDLU == 0123
    let mut player_pos = starting_pos;
    for instruction in path.iter() {
        match instruction {
            Step::L => facing.rotate_right(1),
            Step::R => facing.rotate_left(1),
            Step::Steps(n) => {
                for _ in 0..*n {
                    let new_pos = step_forward(board, player_pos, facing[0]);
                    if new_pos == player_pos {
                        // blocked by #
                        break;
                    }
                    player_pos = new_pos;
                    if log_enabled!(Level::Trace) {
                        pretty_print(board, player_pos, facing[0]);
                    }
                }
            }
        }
        if log_enabled!(Level::Debug) {
            pretty_print(board, player_pos, facing[0]);
            // pretty_print_camera(board, player_pos, facing[0]);
        }
    }
    (player_pos, facing[0])
}

fn step_forward(board: &mut Board, (x, y): (usize, usize), facing: usize) -> (usize, usize) {
    let width = board[0].len() - 1;
    let height = board.len() - 1;
    let (mut new_x, mut new_y) = match facing {
        0 => {
            if x == width {
                (0, y)
            } else {
                (x + 1, y)
            }
        }
        1 => {
            if y == height {
                (x, 0)
            } else {
                (x, y + 1)
            }
        }
        2 => {
            if x == 0 {
                (width, y)
            } else {
                (x - 1, y)
            }
        }
        3 => {
            if y == 0 {
                (x, height)
            } else {
                (x, y - 1)
            }
        }
        _ => panic!(),
    };

    (new_x, new_y) = match board[new_y][new_x] {
        '.' => (new_x, new_y),
        '#' => (x, y),
        BLOCK => {
            match facing {
                0 => {
                    // right
                    let wrapped_x = board[y].iter().position(|c| c != &BLOCK).unwrap();
                    if board[y][wrapped_x] == '#' {
                        (x, y)
                    } else {
                        (wrapped_x, y)
                    }
                }
                1 => {
                    // down
                    let wrapped_y = board.iter().map(|y| y[x]).position(|c| c != BLOCK).unwrap();
                    if board[wrapped_y][x] == '#' {
                        (x, y)
                    } else {
                        (x, wrapped_y)
                    }
                }
                2 => {
                    // left
                    let wrapped_x = board[y].iter().rposition(|c| c != &BLOCK).unwrap();
                    if board[y][wrapped_x] == '#' {
                        (x, y)
                    } else {
                        (wrapped_x, y)
                    }
                }
                3 => {
                    // up
                    let wrapped_y = board
                        .iter()
                        .map(|y| y[x])
                        .rposition(|c| c != BLOCK)
                        .unwrap();
                    if board[wrapped_y][x] == '#' {
                        (x, y)
                    } else {
                        (x, wrapped_y)
                    }
                }
                _ => panic!(),
            }
        }
        _ => panic!(),
    };
    (new_x, new_y)
}

/// returns (board, starting_position, path)
fn parse_input(input: &str) -> (Vec<Vec<char>>, (usize, usize), Vec<Step>) {
    let (map, path_str) = input.split_once("\n\n").unwrap();

    let width = map.lines().map(|l| l.len()).max().unwrap();
    let board: Vec<Vec<char>> = map
        .lines()
        .map(|line| {
            let mut row = vec![BLOCK; width];
            for (i_x, c) in line.chars().enumerate() {
                if c == '.' || c == '#' {
                    row[i_x] = c;
                }
            }
            row
        })
        .collect();
    let starting_x = board[0].iter().position(|c| *c != BLOCK).unwrap();

    let mut path_iter = path_str.lines().next().unwrap().chars();
    let mut path: Vec<Step> = vec![];
    while let Some(c) = path_iter.next() {
        match c {
            'L' => path.push(Step::L),
            'R' => path.push(Step::R),
            first_digit => {
                let remaining_digits = path_iter
                    .take_while_ref(|c| c.is_numeric())
                    .collect::<String>();
                // surely we can improve this with peek (at the while let)?
                let steps = format!("{first_digit}{remaining_digits}")
                    .parse::<usize>()
                    .unwrap();
                path.push(Step::Steps(steps));
            }
        }
    }
    (board, (starting_x, 0), path)
}

fn pretty_print(board: &[Vec<char>], player: (usize, usize), facing: usize) {
    let millis = time::Duration::from_millis(200);
    thread::sleep(millis);
    for (i_y, row) in board.iter().enumerate() {
        for (i_x, col) in row.iter().enumerate() {
            if player == (i_x, i_y) {
                let icon = match facing {
                    0 => ">",
                    1 => "v",
                    2 => "<",
                    3 => "^",
                    _ => panic!(),
                };
                print!("{icon}");
            } else {
                print!("{col}");
            }
        }
        println!();
    }
    println!();
    println!();
}

fn pretty_print_camera(board: &[Vec<char>], player @ (_, y): (usize, usize), facing: usize) {
    let ten_millis = time::Duration::from_millis(200);
    let now = time::Instant::now();
    thread::sleep(ten_millis);
    for (i_y, row) in board.iter().enumerate() {
        if y > 30 {
            if y > (173 - 30) {
                if i_y < y - 50 {
                    continue;
                }
            } else if i_y < y - 10 {
                continue;
            }
        }
        if i_y > y + 20 {
            break;
        }
        for (i_x, col) in row.iter().enumerate() {
            if player == (i_x, i_y) {
                let icon = match facing {
                    0 => ">",
                    1 => "v",
                    2 => "<",
                    3 => "^",
                    _ => panic!(),
                };
                print!("{icon}");
            } else {
                print!("{col}");
            }
        }
        println!();
    }
    println!();
    println!();
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
"#;

    #[test]
    pub fn test_one() {
        env_logger::init();
        assert_eq!(solve_part_1(EXAMPLE_INPUT), 6032);
    }
}
