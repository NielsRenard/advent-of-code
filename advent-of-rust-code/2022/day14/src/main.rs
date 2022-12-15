#![allow(dead_code, unused_mut, unused_variables)]

pub use std::collections::HashMap;
use std::{convert::Infallible, str::FromStr, thread, time};

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/14.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {answer_1}");
}

fn solve_part_one(input: &str) -> usize {
    let (mut map, source): (Vec<Vec<char>>, usize) = parse_input(input);
    pour_sand_till_overflow(&mut map);
    map.into_iter().flatten().filter(|&c| c == 'o').count()
}

fn pour_sand_till_overflow(mut map: &mut Vec<Vec<char>>) {
    // for n in 0..=858 {
    let mut sand_count = 0;
    loop {
        simulate_sand_drop(map);
        let new_count = map.iter_mut().flatten().filter(|c| **c == 'o').count();
        if sand_count == new_count {
            println!("Sand rolling into the void");
            break;
        } else {
            sand_count = new_count;
        }
    }
}

/// simulates a sand drop falling
fn simulate_sand_drop(mut map: &mut Vec<Vec<char>>) {
    let source_x = map[0].iter().position(|&c| c == '+').unwrap();

    let (map_width, map_height) = (map[0].len(), map.len());
    let (mut x, mut y): (usize, usize) = (source_x, 1);
    map[y][x] = 'o';
    let mut falling = true;
    while falling {
        pretty_print(map);

        if y + 1 == map_height {
            println!("bottom of map reached");
            map[y][x] = '.';
            break;
        }
        if x == 0 || x == map_width {
            println!("side of map reached");
            map[y][x] = '.';
            break;
        }

        if map[y + 1][x] == '.' {
            map[y][x] = '.';
            y += 1;
            map[y][x] = 'o';
        } else if map[y + 1][x - 1] == '.' {
            map[y][x] = '.';
            y += 1;
            x -= 1;
            map[y][x] = 'o';
        } else if map[y + 1][x + 1] == '.' {
            map[y][x] = '.';
            y += 1;
            x += 1;
            map[y][x] = 'o';
        } else {
            falling = false;
        }
    }

}

fn parse_input(input: &str) -> (Vec<Vec<char>>, usize) {
    let rocks = input
        .lines()
        .flat_map(|line| {
            let point_str = line.split(" -> ").collect::<Vec<&str>>();
            let mut windows = point_str.windows(2);
            let mut rocks: Vec<Vec<Point>> = vec![];
            while let Some(&[w1, w2]) = windows.next() {
                let a = Point::from_str(w1).unwrap();
                let b = Point::from_str(w2).unwrap();
                rocks.push(generate_coordinates_from_line(&a, &b));
                rocks.dedup();
            }
            rocks.into_iter().flatten()
        })
        .collect::<Vec<Point>>();

    let x_min = rocks.iter().map(|Point(x, _)| x).min().unwrap().to_owned();
    let x_max = rocks.iter().map(|Point(x, _)| x).max().unwrap().to_owned();
    let y_min = rocks.iter().map(|Point(_, y)| y).min().unwrap().to_owned();
    let y_max = rocks.iter().map(|Point(_, y)| y).max().unwrap().to_owned();

    let mut map: Vec<Vec<char>> = vec![vec!['.'; x_max - x_min + 1]; y_max + 1];
    let source_x = 500 - x_min;
    map[0][source_x] = '+';
    for &Point(x, y) in &rocks {
        map[y][x - x_min] = '#';
    }
    (map, source_x)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point(usize, usize);

impl FromStr for Point {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s
            .split_once(',')
            .map(|(x, y)| (x.parse::<usize>().unwrap(), y.parse::<usize>().unwrap()))
            .unwrap();
        Ok(Point(x, y))
    }
}

type Line = (Point, Point);

// reused this from 2021 day 5
fn generate_coordinates_from_line(point_a: &Point, point_b: &Point) -> Vec<Point> {
    let mut direction;
    if point_a.0 == point_b.0 {
        direction = "vertical";
    } else {
        direction = "horizontal";
    }

    let (min_x, max_x) = if point_a.0 < point_b.0 {
        (point_a.0, point_b.0)
    } else {
        (point_b.0, point_a.0)
    };
    let (min_y, max_y) = if point_a.1 < point_b.1 {
        (point_a.1, point_b.1)
    } else {
        (point_b.1, point_a.1)
    };

    let mut covered_xs: Vec<usize> = vec![];
    let mut covered_ys: Vec<usize> = vec![];
    if direction == "horizontal" {
        covered_xs = (min_x..max_x + 1).collect();
        covered_ys = std::iter::repeat(min_y).take(covered_xs.len()).collect();
    } else if direction == "vertical" {
        covered_ys = (min_y..max_y + 1).collect();
        covered_xs = std::iter::repeat(min_x).take(covered_ys.len()).collect();
    }

    std::iter::zip(&covered_xs, &covered_ys)
        .into_iter()
        .map(|(x, y)| Point(*x, *y))
        .collect()
}

/// `⬛⬛⬛⬛⬛⬛🔽⬛⬛⬛ 0`  
/// `⬛⬛⬛⬛⬛⬛⬛⬛⬛⬛ 1`  
/// `⬛⬛⬛⬛⬛⬛🙂⬛⬛⬛ 2`  
/// `⬛⬛⬛⬛⬛🙂🙂🙂⬛⬛ 3`  
/// `⬛⬛⬛⬛⬜🙂🙂🙂⬜⬜ 4`  
/// `⬛⬛⬛🙂⬜🙂🙂🙂⬜⬛ 5`  
/// `⬛⬛⬜⬜⬜🙂🙂🙂⬜⬛ 6`  
/// `⬛⬛⬛⬛🙂🙂🙂🙂⬜⬛ 7`  
/// `🙂🙂⬛🙂🙂🙂🙂🙂⬜⬛ 8`  
/// `⬜⬜⬜⬜⬜⬜⬜⬜⬜⬛ 9`  
fn pretty_print(map: &[Vec<char>]) {
    let ten_millis = time::Duration::from_millis(100);
    let now = time::Instant::now();
    thread::sleep(ten_millis);
    for (i, row) in map.iter().enumerate() {
        // if i < 2 { continue;}
        // if i > 47 { break;}
        for col in row {
            if col == &'#' {
                print!("⬜");
            } else if col == &'.' {
                print!("⬛");
            }
            else if col == &'o' {
                print!("🙂");
            }
            else if col == &'+' {
                print!("🔽");
            }
            // print!("{col}");
        }
        println!(" {i}");
    }
    println!();
    println!();
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
"#;

    #[test]
    fn test_one() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 24);
    }
}
