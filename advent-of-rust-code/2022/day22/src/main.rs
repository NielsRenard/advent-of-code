#![feature(result_option_inspect, string_extend_from_within)]
#![allow(dead_code)]

use std::{thread, time};

use itertools::Itertools;
use log::Level;

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    // std::fs::read_to_string("../../data/2022/22_empty.input").unwrap()
    std::fs::read_to_string("../../data/2022/22.input").unwrap()
}
const BLOCK: char = '⬜';

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
    // let answer_1: usize = solve_part_1(&input_from_file());
    // println!("part 1: {answer_1}");
    let answer_2: usize = solve_part_2(&input_from_file());
    println!("part 2: {answer_2}");
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
                    let new_pos = step_forward_wrapping(board, player_pos, facing[0]);
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

fn step_forward_wrapping(
    board: &mut Board,
    (x, y): (usize, usize),
    facing: usize,
) -> (usize, usize) {
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

fn solve_part_2(input: &str) -> usize {
    let (mut board, starting_pos, path) = parse_input(input);
    // println!("starting pos {starting_pos:?}");
    // println!("initial board:");
    pretty_print(&board, starting_pos, 0);

    let (end_pos @ (x, y), end_facing) = follow_path_part_two(&mut board, starting_pos, path);
    println!("{end_pos:?}, {end_facing}");
    (y + 1) * 1000 + (x + 1) * 4 + end_facing
}

fn follow_path_part_two(
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
                if log_enabled!(Level::Trace) {
                    // pretty_print(board, player_pos, facing[0]);
                    // pretty_print_camera(board, player_pos, facing[0]);
                }

                for _ in 0..*n {
                    let (new_pos, new_facing) = step_forward_part_two(board, player_pos, facing[0]);
                    match new_facing {
                        0 => {
                            facing = [0, 1, 2, 3];
                        }
                        1 => {
                            facing = [1, 2, 3, 0];
                        }
                        2 => {
                            facing = [2, 3, 0, 1];
                        }
                        3 => {
                            facing = [3, 0, 1, 2];
                        }
                        _ => panic!(),
                    };
                    if new_pos == player_pos {
                        // blocked by #
                        break;
                    }

                    player_pos = new_pos;
                    if log_enabled!(Level::Trace) {
                        // pretty_print(board, player_pos, facing[0]);
                        pretty_print_camera(board, player_pos, facing[0]);
                    }
                }
            }
        }
        // debug!("{player_pos:?}, f{}", facing[0]);
    }
    (player_pos, facing[0])
}

/*
My cube layout, (no thought into number assignment)

          4 4 4 4 4 1 1 1 1 1
          4 4 4 4 4 1 1 1 1 1
          4 4 4 4 4 1 1 1 1 1
          4 4 4 4 4 1 1 1 1 1
          4 4 4 4 4 1 1 1 1 1
          6 6 6 6 6
          6 6 6 6 6
          6 6 6 6 6
          6 6 6 6 6
          6 6 6 6 6
3 3 3 3 3 2 2 2 2 2
3 3 3 3 3 2 2 2 2 2
3 3 3 3 3 2 2 2 2 2
3 3 3 3 3 2 2 2 2 2
3 3 3 3 3 2 2 2 2 2
5 5 5 5 5 
5 5 5 5 5 
5 5 5 5 5 
5 5 5 5 5 
5 5 5 5 5 
5 5 5 5 5 
*/

fn step_forward_part_two(
    board: &mut Board,
    (x, y): (usize, usize),
    facing: usize,
) -> ((usize, usize), usize) {
    let mut new_facing = facing;
    let (mut new_x, mut new_y) = match facing {
        0 => {
            if x == 149 && y < 50 {
                // 1 to 2
                new_facing = 2;
                (99, 149 - y)
            } else if x == 99 && y > 49 && y < 100 {
                // 6 to 1
                new_facing = 3;
                (y + 50, 49)
            } else if x == 99 && y > 99 && y < 150 {
                // 2 to 1
                new_facing = 2;
                (149, 149 - y)
            } else if x == 49 && y > 149 && y <= 199 {
                // 5 to  2
                new_facing = 3;
                (y - 100, 149)
            } else {
                (x + 1, y)
            }
        }
        1 => {
            if y == 49 && x > 99 && x <= 149 {
                // 1 to 6
                new_facing = 2;
                (99, x - 50)
            } else if y == 149 && x > 49 && x <= 99 {
                // 2 to 5
                new_facing = 2;
                (49, x + 100)
            } else if y == 199 && x < 50 {
                // 5 to 1
                new_facing = 1;
                (x + 100, 0)
            } else {
                (x, y + 1)
            }
        }
        2 => {
            if x == 50 && y < 50 {
                // 4 to 3
                new_facing = 0;
                (0, 149 - y)
            } else if x == 0 && y > 149 && y <= 199 {
                // 5 to 4
                new_facing = 1;
                (y - 100, 0)
            } else if x == 0 && (99..=149).contains(&y) {
                // 3 to 4
                new_facing = 0;
                (50, 149 - y)
            } else if x == 50 && (49..=99).contains(&y) {
                // 6 to 3
                new_facing = 1;
                (y - 50, 100)
            } else {
                (x - 1, y)
            }
        }
        3 => {
            if y == 0 && x > 49 && x <= 99 {
                // 4 to 5
                new_facing = 0;
                (0, x + 100)
            } else if y == 0 && x > 99 && x <= 149 {
                // 1 to  5
                new_facing = 3;
                (x - 100, 199)
            } else if y == 100 && x <= 49 {
                // 3 to 6
                new_facing = 0;
                (50, x + 50)
            } else {
                (x, y - 1)
            }
        }
        _ => panic!(),
    };

    (new_x, new_y) = match board[new_y][new_x] {
        '.' => (new_x, new_y),
        '#' => {
            new_facing = facing;
            (x, y)
        }
        _ => panic!(),
    };
    ((new_x, new_y), new_facing)
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


///`⬜⬜⬜        🎄                          `  
///`⬜⬜⬜                                🎄  `  
///`⬜⬜⬜                    🎄🎄            `  
///`⬜⬜⬜                🎄        🎄🎄      `  
///`⬜⬜⬜      🎄🎄                          `  
///`⬜⬜⬜                                🎄  `  
///`⬜⬜⬜            🎄                      `  
///`⬜⬜⬜    🎄                              `  
///`⬜⬜⬜                        🎄          `  
///`⬜⬜⬜            🎄    🎄                `  
///`⬜⬜⬜              👇        🎄          `  
///`⬜⬜⬜          🎄                        `  
///`⬜⬜⬜                            🎄      `  
///`⬜⬜⬜🎄  🎄    🎄            🎄          `  
///`⬜⬜⬜                            🎄      `  
///`⬜⬜⬜🎄              🎄            🎄    `  
///`⬜⬜⬜        🎄🎄                        `  
///`⬜⬜⬜🎄  🎄      🎄                      `  
///`⬜⬜⬜    🎄                  🎄          `  
///`⬜⬜⬜        🎄        🎄  🎄            `  
///`⬜⬜⬜🎄    🎄                            `  
fn pretty_print_camera(board: &[Vec<char>], player @ (x, y): (usize, usize), facing: usize) {
    let ten_millis = time::Duration::from_millis(80);
    thread::sleep(ten_millis);
    for (i_y, row) in board.iter().enumerate() {
        if i_y > y + 10 {
            break;
        }
        for (i_x, col) in row.iter().enumerate() {
            if x >= 10 && i_x < x - 10 {
                continue;
            }
            if i_x > x + 10 {
                // println!("X: {i_x}, {x}");
                break;
            }

            if player == (i_x, i_y) {
                let icon = match facing {
                    0 => "👉",
                    1 => "👇",
                    2 => "👈",
                    3 => "👆",
                    _ => panic!(),
                };
                print!("{icon}");
            } else if *col == '.' {
                print!("  ");
            } else if *col == '#' {
                print!("🎄");
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
