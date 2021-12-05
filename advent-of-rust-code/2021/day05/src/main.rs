#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip)]

use std::{
    collections::{hash_map, HashMap},
    hash::Hash,
    ops::{Add, Mul, Sub},
};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/5.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    // execution took 9.7ms

    // let answer_2 = solve_part_two(&input_from_file());
    // println!("part 2: {}", answer_2);
    // execution took ?
}

type Line = (Point, Point);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

fn solve_part_one(input: &str) -> i32 {
    let lines = input.lines().into_iter().map(|s| parse_line_from_string(s));
    let mut map: HashMap<Point, i32> = HashMap::new();
    for line in lines {
        mark_on_map(line, &mut map)
    }
    // println!("map {:?}", map);
    let dangerous_points: HashMap<&Point, &i32> = map.iter().filter(|(k, v)| v > &&1).collect();
    dangerous_points.len() as i32
}

fn mark_on_map((point_a, point_b): Line, mut map: &mut HashMap<Point, i32>) {
    let points: Vec<Point> = generate_coordinates_from_line((point_a, point_b));
    for point in points {
        match map.get(&point) {
            Some(val) => {
                let deref_val = *val;
                // println!("incrementing point {:?} where value was: {:?}", point, deref_val);
                map.insert(point, 1 + deref_val);
            }
            None => {
                // println!("new mark at point{:?} ", point);
                map.insert(point, 1);
            }
        }
    }
    // println!("map {:?}", map);
}

fn parse_line_from_string(s: &str) -> Line {
    let split: Vec<&str> = s.split_whitespace().collect();
    let first_part: Vec<&str> = split[0].split(',').collect();
    let _arrow: &str = split[1];
    let third_part: Vec<&str> = split[2].split(',').collect();
    // println!("third {:?}", third_part);
    let point_a: Point = Point {
        x: first_part[0].parse().unwrap(),
        y: first_part[1].parse().unwrap(),
    };
    let point_b: Point = Point {
        x: third_part[0].parse().unwrap(),
        y: third_part[1].parse().unwrap(),
    };
    (point_a, point_b)
}

fn generate_coordinates_from_line((point_a, point_b): Line) -> Vec<Point> {
    let mut direction = "";
    if point_a.x == point_b.x {
        direction = "vertical";
    }
    if point_a.y == point_b.y {
        direction = "horizontal";
    }
    if direction == "" {
        direction = "diagonal";
    }

    let (min_x, max_x) = if point_a.x < point_b.x {
        (point_a.x, point_b.x)
    } else {
        (point_b.x, point_a.x)
    };
    let (min_y, max_y) = if point_a.y < point_b.y {
        (point_a.y, point_b.y)
    } else {
        (point_b.y, point_a.y)
    };

    // println!("{:?}", direction);
    // println!("{:?}", min_x);
    // println!("{:?}", max_x);
    // println!("{:?}", min_y);
    // println!("{:?}", max_y);

    let mut covered_xs: Vec<i32> = vec![];
    let mut covered_ys: Vec<i32> = vec![];
    if direction == "horizontal" {
        covered_xs = (min_x..max_x + 1).collect();
        covered_ys = std::iter::repeat(min_y).take(covered_xs.len()).collect();
    } else if direction == "vertical" {
        covered_ys = (min_y..max_y + 1).collect();
        covered_xs = std::iter::repeat(min_x).take(covered_ys.len()).collect();
    }
    // println!("{:?}", covered_xs);
    // println!("{:?}", covered_ys);
    std::iter::zip(&covered_xs, &covered_ys)
        .into_iter()
        .map(|(x, y)| Point { x: *x, y: *y })
        .collect()
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2";
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 5);
    }
    // #[test]
    // fn test_2() {
    //     assert_eq!(solve_part_two(EXAMPLE_INPUT), 1924);
    // }
}
