#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use std::{
    collections::HashMap,
    hash::Hash,
    ops::{Range, RangeBounds, RangeInclusive},
};

use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/22.input").unwrap()
}

fn main() {
    print_time!("execution");
    let input = &input_from_file();
    let answer_1 = solve_part_one(&input);
    println!("part 1: {:?}", answer_1);
    let answer_2 = solve_part_two(&input);
    println!("part 1: {:?}", answer_2);
    // execution took: 0.16ms
}

fn solve_part_one(input: &str) -> u64 {
    let reboot_steps: Vec<RebootStep> = parse(&input);
    let mut reactor: HashMap<(i32, i32, i32), bool> = HashMap::new();
    fn within_fifty(number: i32) -> bool {
        number <= 50 && number >= -50
    }
    let only_within_50_range: Vec<RebootStep> = reboot_steps
        .iter()
        .filter(|(_, ranges)| {
            within_fifty(*ranges.0.start())
                && within_fifty(*ranges.0.end())
                && within_fifty(*ranges.1.start())
                && within_fifty(*ranges.1.end())
                && within_fifty(*ranges.2.start())
                && within_fifty(*ranges.2.end())
        })
        .cloned()
        .collect_vec();

    let cubes = &only_within_50_range
        .iter()
        .map(generate_cubes)
        .collect_vec();
    for cube in cubes {
        toggle_cubes(&mut reactor, cube);
    }
    reactor.values().filter(|v| **v == true).count() as u64
}

fn solve_part_two(input: &str) -> u64 {
    let reboot_steps: Vec<RebootStep> = parse(&input);
    let mut reactor: HashMap<(i32, i32, i32), bool> = HashMap::new();
    let cubes: &Vec<HashMap<(i32, i32, i32), bool>> =
        &reboot_steps.iter().map(generate_cubes).collect_vec();
    for cube in cubes {
        toggle_cubes(&mut reactor, cube);
    }
    reactor.values().filter(|v| **v == true).count() as u64
}

type RebootStep = (
    bool,
    (
        RangeInclusive<i32>,
        RangeInclusive<i32>,
        RangeInclusive<i32>,
    ),
);

fn toggle_cubes(
    reactor: &mut HashMap<(i32, i32, i32), bool>,
    cubes: &HashMap<(i32, i32, i32), bool>,
) {
    for (point, on_or_off) in cubes {
        let result = reactor.insert(*point, *on_or_off);
        // match result {
        //     Some(old_value) => { println!("Update cube {:?}: old: {}, new: {}", point, old_value, on_or_off);}
        //     None => {           println!("New cube    {:?}: {:?}", point, on_or_off); },
        // }
    }
}

fn generate_cubes(
    (on_or_off, (x_range, y_range, z_range)): &RebootStep,
) -> HashMap<(i32, i32, i32), bool> {
    let mut cubes = HashMap::new();
    for y in y_range.clone() {
        for x in x_range.clone() {
            for z in z_range.clone() {
                cubes.insert((x, y, z), on_or_off.clone());
            }
        }
    }
    cubes
}

fn parse(input: &str) -> Vec<RebootStep> {
    input
        .lines()
        .map(|l| l.split(' '))
        .map(|mut on_and_ranges| {
            (
                on_and_ranges.next().unwrap(),
                on_and_ranges.next().unwrap().split(',').collect_vec(),
            )
        })
        .map(|(on, xyz)| {
            (
                on == "on",
                (
                    string_to_range(&(xyz[0])[2..]),
                    string_to_range(&(xyz[1])[2..]),
                    string_to_range(&(xyz[2])[2..]),
                ),
            )
        })
        .collect()
}

fn string_to_range(string: &str) -> RangeInclusive<i32> {
    let nums: Vec<i32> = string.split("..").map(|s| s.parse().unwrap()).collect_vec();
    nums[0]..=nums[1]
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10";

    const LARGER_EXAMPLE_INPUT: &str = "on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 39);
    }

    #[test]
    fn test_1_larger() {
        assert_eq!(solve_part_one(LARGER_EXAMPLE_INPUT), 590784);
    }
}
