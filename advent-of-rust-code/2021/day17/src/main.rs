#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use std::ops::Add;

use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn main() {
    print_time!("execution");
    // target area: x=155..215, y=-132..-72
    let target_area = (155, 215, -132, -72);
    let answer_1 = solve_part_one(target_area);
    println!("part 1: {}", answer_1);
    let answer_2 = solve_part_two(target_area);
    // 4969 too low
    println!("part 2: {}", answer_2);
    // execution took 3.18ms

}

fn step(
    pos @ (pos_x, pos_y): (i32, i32),
    target @ (x_min, x_max, y_min, y_max): (i32, i32, i32, i32),
    velocity @ (vel_x, vel_y): (i32, i32),
) -> ((i32, i32), bool, (i32, i32)) {
    let new_pos: (i32, i32) = (pos_x + vel_x, pos_y + vel_y);
    let new_vel_x: i32 = if vel_x > 0 { vel_x - 1 } else { 0 };
    let new_vel_y: i32 = vel_y - 1;
    let inside_target =
        new_pos.0 >= x_min && new_pos.0 <= x_max && new_pos.1 >= y_min && new_pos.1 <= y_max;
    (new_pos, inside_target, (new_vel_x, new_vel_y))
}

fn solve_part_one(target @ (x_min, x_max, y_min, y_max): (i32, i32, i32, i32)) -> i32 {
    let init_pos = (0, 0);

    let mut velocities: Vec<(i32, i32)> = vec![];
    for x in 0..=x_max {
        for y in y_min..200 {
            velocities.push((x, y));
        }
    }
    let mut y_max_all = (0, (0, 0));

    for (x, y) in velocities {
        let mut in_target: bool;
        let (mut pos, mut vel) = (init_pos, (x, y));
        let mut y_max = 0;
        let mut hit: bool = false;
        while pos.0 <= x_max && pos.1 >= y_min {
            (pos, in_target, vel) = step(pos, target, vel);
            if pos.1 > y_max {
                y_max = pos.1
            }
            if in_target {
                hit = true;
                break;
            }
        }
        if hit && y_max > y_max_all.0 {
            y_max_all = (y_max, (x, y))
        }
    }
    println!(
        "highest y was {:?} for velocity {:?}",
        y_max_all.0, y_max_all.1
    );
    y_max_all.0
}

fn solve_part_two(target @ (x_min, x_max, y_min, y_max): (i32, i32, i32, i32)) -> usize {
    let init_pos = (0, 0);

    let mut velocities: Vec<(i32, i32)> = vec![];
    for x in 0..=x_max {
        for y in y_min..200 {
            velocities.push((x, y));
        }
    }

    let mut on_target_velocities: Vec<(i32, i32)> = vec![];
    for (x, y) in velocities {
        let mut in_target: bool;
        let (mut pos, mut vel) = (init_pos, (x, y));
        while pos.0 <= x_max && pos.1 >= y_min {
            (pos, in_target, vel) = step(pos, target, vel);
            if in_target {
                on_target_velocities.push((x, y));
                break;
            }
        }
    }
    on_target_velocities.len()
}

// fn step(pos@(pos_x, pos_y): (i32, i32), x_min: i32, x_max: i32, y_min: i32, y_max: i32) {
//     println!("{:?}", pos);
// }

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "D2FE28";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one((20, 30, -10, -5)), 45);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two((20, 30, -10, -5)), 112);
    }
}
