#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment)]

use core::{panic, time};
use std::{
    borrow::BorrowMut,
    char::from_digit,
    collections::{hash_map, HashMap, HashSet},
    hash::Hash,
    ops::{Add, Deref, Mul, RangeBounds, Sub},
    thread,
};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/11.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file(), 100);
    println!("part 1: {}", answer_1);
    // part 1 execution took 0.69ms
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    // part 2 execution took 1.88ms
    // execution took 2.57ms
}

type Matrix = Vec<Vec<u32>>;

fn solve_part_one(input: &str, steps: u32) -> u64 {
    let mut matrix: Matrix = input
        .lines()
        .into_iter()
        .map(|row| {
            row.chars()
                .into_iter()
                .map(|c| c.to_digit(10).unwrap())
                .collect()
        })
        .collect();

    let mut total_flashes: u64 = 0;
    for i in 1..=steps {
        let adjacent = get_adjacent(0, 0, &matrix);
        let flashed: u32 = advance_step(&mut matrix);
        total_flashes += flashed as u64;

        // uncomment to visualize in terminal
        // if i % 1 == 0 {
        //     println!("After {} steps:", i);
        //     combination_print(&matrix);
        //     let millis = time::Duration::from_millis(15);
        //     thread::sleep(millis);
        // }
    }
    total_flashes
}

fn solve_part_two(input: &str) -> u64 {
    let mut matrix: Matrix = input
        .lines()
        .into_iter()
        .map(|row| {
            row.chars()
                .into_iter()
                .map(|c| c.to_digit(10).unwrap())
                .collect()
        })
        .collect();

    let mut synchronized_at: u64 = 0;
    for i in 1.. {
        let adjacent = get_adjacent(0, 0, &matrix);
        let flashed: u32 = advance_step(&mut matrix);
        if flashed == 100 {
            println!("All synchronized at {} steps", i);
            synchronized_at = i as u64;
            break;
        }
    }
    synchronized_at
}

fn advance_step(mut matrix: &mut Matrix) -> u32 {
    // "First, the energy level of each octopus increases by 1"
    matrix.iter_mut().for_each(|row| {
        row.iter_mut().for_each(|octopus| {
            let incremented = if *octopus < 9 { *octopus + 1 } else { 0 };
            *octopus = incremented;
        })
    });
    // Then, any octopus with an energy level greater than 9 flashes.
    let mut flashed: Vec<(usize, usize)> = vec![];
    for (y, row) in matrix.iter().enumerate() {
        let width = row.len();
        for x in 0..width {
            let octopus = matrix[y][x];
            if octopus == 0 {
                flashed.push((x, y));
            }
        }
    }

    // This increases the energy level of all adjacent octopuses by 1
    let mut keep_going = true;
    let mut idx = 0;
    while keep_going && !flashed.is_empty() {
        let (x, y) = flashed[idx];
        let adjacent_to_flashed = get_adjacent(x, y, matrix);
        for (x, y, octopus) in adjacent_to_flashed {
            if flashed.contains(&(x, y)) {
                // println!("x:{},y:{} already flashed this step", x, y)
            } else {
                let incremented = if octopus < 9 { octopus + 1 } else { 0 };
                matrix[y][x] = incremented;
                if incremented == 0 {
                    flashed.push((x, y));
                }
            }
        }
        if idx < flashed.len() - 1 {
            idx += 1;
            // print!("continue");
        } else {
            // print!("stop");
            keep_going = false;
        }
    }
    flashed.len().try_into().unwrap()
}

fn print(matrix: &Matrix) {
    let width = matrix[0].len();
    matrix.iter().for_each(|row| {
        row.iter().enumerate().for_each(|(idx, n)| {
            if idx + 1 < width {
                if *n == 0 {
                    print!(" 0 ")
                } else {
                    print!(" {} ", n)
                }
            } else if *n == 0 {
                println!(" 0")
            } else {
                println!(" {}", n)
            }
        })
    })
}

fn pretty_print(matrix: &Matrix) {
    let width = matrix[0].len();
    matrix.iter().for_each(|row| {
        row.iter().enumerate().for_each(|(idx, n)| {
            if idx + 1 < width {
                if *n == 0 {
                    print!("💡 ")
                } else {
                    print!("🐙 ")
                }
            } else if *n == 0 {
                println!("💡 ")
            } else {
                println!("🐙 ")
            }
        })
    })
}

fn combination_print(matrix: &Matrix) {
    let width = matrix[0].len();
    matrix.iter().for_each(|row| {
        row.iter().enumerate().for_each(|(idx, n)| {
            if *n == 0 {
                print!(" 0")
            } else {
                print!(" {}", n)
            }
        });
        row.iter().enumerate().for_each(|(idx, n)| {
            if idx == 0 {
                print!(" | ")
            }
            if idx + 1 < width {
                if *n == 0 {
                    print!("💡")
                } else {
                    print!("🐙")
                }
            } else if *n == 0 {
                println!("💡 ")
            } else {
                println!("🐙 ")
            }
        });
    })
}

fn get_adjacent(x: usize, y: usize, matrix: &Matrix) -> Vec<(usize, usize, u32)> {
    // also returns diagonals
    // top_left    , top     , topright
    // left        , nothing , right
    // bottom_left , bottom  , bottom_right

    if matrix.get(y).is_none() {
        panic!("row does not exist")
    };
    let mut adjacent_coords: Vec<(usize, usize, u32)> = vec![];
    let row = &matrix[y];
    let own_value = row[x];
    if y > 0 {
        if x > 0 {
            if let Some(value_top_left) = matrix.get(y - 1).unwrap().get(x - 1) {
                adjacent_coords.push((x - 1, y - 1, *value_top_left))
            }
        }
        if let Some(row_above) = matrix.get(y - 1) {
            let value_above = row_above.get(x).unwrap();
            adjacent_coords.push((x, y - 1, *value_above))
        }
        if let Some(value_top_right) = matrix.get(y - 1).unwrap().get(x + 1) {
            adjacent_coords.push((x + 1, y - 1, *value_top_right))
        }
    }

    if x > 0 {
        if let Some(value_left) = row.get(x - 1) {
            adjacent_coords.push((x - 1, y, *value_left))
        }
    }
    if let Some(value_right) = row.get(x + 1) {
        adjacent_coords.push((x + 1, y, *value_right));
    }
    if let Some(row_below) = matrix.get(y + 1) {
        let value_below = row_below.get(x).unwrap();
        adjacent_coords.push((x, y + 1, *value_below));
        if x > 0 {
            if let Some(value_bottom_left) = matrix.get(y + 1).unwrap().get(x - 1) {
                adjacent_coords.push((x - 1, y + 1, *value_bottom_left))
            }
        }
        if let Some(value_bottom_right) = matrix.get(y + 1).unwrap().get(x + 1) {
            adjacent_coords.push((x + 1, y + 1, *value_bottom_right))
        }
    }

    adjacent_coords
}

#[cfg(test)]
mod tests {

    use super::*;
    const LITTLE_EXAMPLE: &str = "11111
19991
19191
19991
11111";

    const EXAMPLE_INPUT: &str = "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT, 100), 1656);
    }
    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 195);
    }
}
