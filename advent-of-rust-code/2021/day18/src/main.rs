#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use std::ops::Add;

use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/18.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(input_from_file().lines().next().unwrap());
    println!("part 1: {}", answer_1);
    // let answer_2 = solve_part_two(input_from_file().lines().next().unwrap());
    // println!("part 2: {}", answer_2);
    // execution took 2.49ms
}

fn solve_part_one(input: &str) -> i64 {
    1
}

fn add_numbers() {
    // To add two snailfish numbers, form a pair from the left and
    // right parameters of the addition operator. For example:
    //  [1,2] + [[3,4],5] becomes
    // [[1,2],  [[3,4],5]]
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 4140);
    }

    #[test]
    fn test_1_a() {
        assert_eq!(solve_part_one("[[1,2],[[3,4],5]]"), 143);
    }

    #[test]
    fn test_1_b() {
        assert_eq!(solve_part_one("[[1,2],[[3,4],5]]"), 143);
    }
    #[test]
    fn test_1_c() {
        assert_eq!(solve_part_one("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"), 1384);
    }
    #[test]
    fn test_1_d() {
        assert_eq!(solve_part_one("[[[[1,1],[2,2]],[3,3]],[4,4]]"), 445);
    }
    #[test]
    fn test_1_e() {
        assert_eq!(solve_part_one("[[[[3,0],[5,3]],[4,4]],[5,5]]"), 791);
    }
    #[test]
    fn test_1_f() {
        assert_eq!(solve_part_one("[[[[5,0],[7,4]],[5,5]],[6,6]]"), 1137);
    }
    #[test]
    fn test_1_g() {
        assert_eq!(
            solve_part_one("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"),
            3488
        );
    }
}
