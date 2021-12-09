#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment)]

use std::{
    char::from_digit,
    collections::{hash_map, HashMap, HashSet},
    hash::Hash,
    ops::{Add, Mul, RangeBounds, Sub},
};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/9.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    // execution took 3.2ms
}

fn solve_part_one(input: &str) -> u32 {
    let heightmap = heightmap(input);
    let mut risk_levels: Vec<u32> = vec![];
    for y in 0..heightmap.len() {
        for x in 0..heightmap.get(0).unwrap().len() {
            let (me, adjacent) = get_self_and_adjacent(x, y, &heightmap);
            if adjacent.iter().all(|adj| &me < adj) {
                risk_levels.push(me + 1);
            }
        }
    }
    risk_levels.iter().sum()
}

type HeightMap = Vec<Vec<u32>>;

fn solve_part_two(input: &str) -> u32 {
    let heightmap = heightmap(input);
    let mut low_points_and_vals: Vec<(usize, usize, u32)> = vec![];
    for y in 0..heightmap.len() {
        for x in 0..heightmap.get(0).unwrap().len() {
            let (me, adjacent) = get_self_and_adjacent(x, y, &heightmap);
            if adjacent.iter().all(|adj| &me < adj) {
                low_points_and_vals.push((x, y, me));
            }
        }
    }

    let mut all_basins: Vec<HashSet<(usize, usize)>> = vec![];
    for (x, y, low_point) in low_points_and_vals {
        let all_adjacent = get_higher_adjacent(x, y, &heightmap);
        let mut basin: HashSet<(usize, usize)> = HashSet::from([(x, y)]);
        for adjacent in all_adjacent {
            let mut keep_going = true;
            let mut higher_adjecent: Vec<(usize, usize)> = vec![adjacent];
            let mut idx = 0;
            while keep_going {
                let current = higher_adjecent[idx];
                let mut new_highers: Vec<(usize, usize)> =
                    get_higher_adjacent(current.0, current.1, &heightmap);

                if !new_highers.is_empty() {
                    higher_adjecent.append(&mut new_highers);
                }
                if idx < higher_adjecent.len() - 1 {
                    idx += 1;
                    // continue
                } else {
                    // stop
                    keep_going = false;
                }
            }
            basin.extend(HashSet::<(usize, usize)>::from_iter(
                higher_adjecent.iter().cloned(),
            ));
        }
        all_basins.push(basin);
    }

    all_basins.sort_by_key(|a| a.len());
    all_basins
        .iter()
        .rev()
        .take(3)
        .map(|b| b.len() as u32)
        .product()
}

fn get_higher_adjacent(x: usize, y: usize, heightmap: &HeightMap) -> Vec<(usize, usize)> {
    if heightmap.get(y).is_none() {
        panic!("row does not exist")
    };
    let mut higher_adjacent: Vec<(usize, usize)> = vec![];
    let row = heightmap.get(y).unwrap();
    let own_value = row.get(x).unwrap();
    if y > 0 {
        if let Some(row_above) = heightmap.get(y - 1) {
            let value_above = row_above.get(x).unwrap();
            if value_above > own_value && value_above != &9 {
                higher_adjacent.push((x, y - 1))
            }
        }
    }
    if x > 0 {
        if let Some(value_left) = row.get(x - 1) {
            if value_left > own_value && value_left != &9 {
                higher_adjacent.push((x - 1, y))
            }
        }
    }

    if let Some(value_right) = row.get(x + 1) {
        if value_right > own_value && value_right != &9 {
            higher_adjacent.push((x + 1, y))
        }
    }
    if let Some(row_below) = heightmap.get(y + 1) {
        let value_below = row_below.get(x).unwrap();
        if value_below > own_value && value_below != &9 {
            higher_adjacent.push((x, y + 1))
        }
    }

    higher_adjacent
}

fn get_self_and_adjacent(x: usize, y: usize, heightmap: &HeightMap) -> (u32, Vec<u32>) {
    if heightmap.get(y).is_none() {
        panic!("row does not exist")
    };
    let mut adjacent: Vec<u32> = vec![];
    let row = heightmap.get(y).unwrap();
    if y > 0 {
        if let Some(row_above) = heightmap.get(y - 1) {
            adjacent.push(*row_above.get(x).unwrap());
        };
    }
    if x > 0 {
        if let Some(value_left) = row.get(x - 1) {
            adjacent.push(*value_left);
        };
    }
    if let Some(value_right) = row.get(x + 1) {
        adjacent.push(*value_right);
    };
    if let Some(row_below) = heightmap.get(y + 1) {
        adjacent.push(*row_below.get(x).unwrap());
    };
    (*row.get(x).unwrap(), adjacent)
}

fn heightmap(input: &str) -> Vec<Vec<u32>> {
    let height_map: Vec<Vec<u32>> = input
        .lines()
        .map(|line| {
            line.chars()
                .into_iter()
                .map(|c| char::to_digit(c, 10).unwrap())
                .collect()
        })
        .collect();
    height_map
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "2199943210
3987894921
9856789892
8767896789
9899965678";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 15);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 1134);
    }
}
