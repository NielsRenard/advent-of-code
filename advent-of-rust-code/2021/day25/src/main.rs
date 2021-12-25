#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use core::time;
use std::{
    collections::{HashMap, VecDeque},
    hash::Hash, thread,
};

use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/25.input").unwrap()
}

fn main() {
    print_time!("execution");
    let input = &input_from_file();
    let answer_1 = solve_part_one(input);
    println!("part 1: {:?}", answer_1);
}

fn solve_part_one(input: &str) -> i64 {
    
    let mut cucumbers: Vec<Vec<char>> = parse(input);
    let y_max = cucumbers.len();
    let x_max = cucumbers.get(0).unwrap().len();
    let mut count = 0;
    loop {
        /*
        I just loop over the list twice, first for the >'s and then for the v's
        */

        let millis = time::Duration::from_millis(30);
        thread::sleep(millis);

        // println!("after {:?} steps:", count); 
        let mut after_east_movement = cucumbers.clone();
        pretty_print_cucumbers(&after_east_movement);
        let mut moved = 0;
        // move east facing
        for y in 0..y_max {
            for x in 0..x_max {
                if cucumbers[y][x] == '>' {
                    if x == x_max-1 {
                        // right edge
                        if cucumbers[y][0] == '.' {
                            after_east_movement[y][x] = '.';
                            after_east_movement[y][0] = '>';
                            moved += 1;
                        }
                    } else if cucumbers[y][x+1] == '.' {
                        after_east_movement[y][x] = '.';
                        after_east_movement[y][x+1] = '>';
                        moved += 1;
                    }
                }
            }
        }

        let mut after_south_movement = after_east_movement.clone();

        // move south facing
        for y in 0..y_max {
            for x in 0..x_max {
                if after_east_movement[y][x] == 'v' {
                    if y == y_max-1 {
                        // bottom edge
                        if after_east_movement[0][x] == '.' {
                            after_south_movement[y][x] = '.';
                            after_south_movement[0][x] = 'v';
                            moved += 1;
                        }
                    } else if after_east_movement[y+1][x] == '.' {
                        after_south_movement[y][x] = '.';
                        after_south_movement[y+1][x] = 'v';
                        moved += 1;
                    }
                }
            }
        }
        cucumbers = after_south_movement;
        count += 1;

        if moved == 0 {
            // println!("nobody moved!"); 
            break;
        }
    }
    count
}



fn parse(input: &str) -> Vec<Vec<char>> {
    input
        .lines()
        .map(|line| {
            line.chars()
                .into_iter()
                .collect()
        })
        .collect()
}

fn pretty_print_cucumbers(sea_floor: &Vec<Vec<char>>) {
    let max_x = &sea_floor[0].len();
    let max_y = sea_floor.len();

    // std::iter::repeat(" ☬").take(*max_x).for_each(|it| print!("{}", it));
    println!();
    println!(); 
    println!(); 

    for y in 0..max_y {
        // println!();
        for x in 0..*max_x {
            let spot = sea_floor[y][x];
            if spot == '.' { print!("   ") } else { print!("{}  ", spot); }
            if x == *max_x-1 {
                println!()
            }
        }
    }
    println!(); 
    println!(); 
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>";

//     const LITTLE_EXAMPLE_INPUT: &str = "...>...
// .......
// ......>
// v.....>
// ......>
// .......
// ..vvv..";
    
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 58);
    }
}
