#![allow(clippy::type_complexity)]

use std::cmp::Ordering;

use serde_json::{
    Value,
    {Value::Array, Value::Number},
};

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/13.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {answer_1}");
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {answer_2}");
}

#[derive(Debug, Clone)]
pub struct Packet(Vec<Value>);

type Pair = (Packet, Packet);

pub fn solve_part_one(input: &str) -> usize {
    let pairs: Vec<Pair> = parse_input(input);
    pairs
        .into_iter()
        .enumerate()
        .filter_map(|(i, (left, right))| arrays_in_order(&left, &right).then_some(i))
        .sum()
}

pub fn solve_part_two(input: &str) -> usize {
    let mut pairs: Vec<Packet> = parse_input_part_two(input);
    pairs.push(Packet::new("[[2]]"));
    pairs.push(Packet::new("[[6]]"));

    pairs.sort();

    pairs
        .into_iter()
        .enumerate()
        .filter(|(_, pair)| pair == &Packet::new("[[2]]") || pair == &Packet::new("[[6]]"))
        .map(|(i, _)| i + 1)
        .product::<usize>()
}

pub fn arrays_in_order(left: &Packet, right: &Packet) -> bool {
    let left_iter = left.0.iter();
    let mut right_iter = right.0.iter();
    let mut is_correct = false;
    for left_value in left_iter {
        match right_iter.next() {
            None => {
                return false;
            }
            Some(right_value) => {
                if left_value == right_value {
                    is_correct = true;
                    continue;
                }
                return correct_order(left_value, right_value);
            }
        }
    }

    if right_iter.next().is_some() {
        return true;
    }
    is_correct
}

/// Checks that the lower value is on the left.
pub fn correct_order(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Number(l_n), Number(r_n)) => l_n.as_u64() < r_n.as_u64(),
        (Array(l_arr), Array(r_arr)) => {
            arrays_in_order(&Packet(l_arr.to_vec()), &Packet(r_arr.to_vec()))
        }
        (Array(l_arr), Number(_)) => {
            let n_as_arr = vec![right.clone()].into_iter().collect::<Vec<Value>>();
            arrays_in_order(&Packet(l_arr.to_vec()), &Packet(n_as_arr))
        }
        (Number(_), Array(right_array)) => {
            let n_as_arr = vec![left.clone()].into_iter().collect::<Vec<Value>>();
            arrays_in_order(&Packet(n_as_arr), &Packet(right_array.to_vec()))
        }
        _ => panic!(),
    }
}

pub fn parse_input(input: &str) -> Vec<Pair> {
    let pairs: Vec<Pair> = input
        .split("\n\n")
        .map(|s| {
            let (left, right) = s.split_once('\n').unwrap();
            (Packet::new(left), Packet::new(right))
        })
        .collect();
    pairs
}

pub fn parse_input_part_two(input: &str) -> Vec<Packet> {
    let pairs = input
        .split("\n\n")
        .flat_map(|s| s.lines())
        .map(Packet::new)
        .collect::<Vec<Packet>>();
    pairs
}

impl Packet {
    pub fn new(s: &str) -> Self {
        Packet(
            serde_json::from_str::<Value>(s)
                .unwrap()
                .as_array()
                .unwrap()
                .to_vec(),
        )
    }
}

impl Eq for Packet {}

impl PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        if arrays_in_order(self, other) {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
"#;

    const EXAMPLE_EXTRA: &str = r#"[1]
[[1],1]
"#;

    #[test]
    fn test_one() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 13);
    }

    #[test]
    fn test_extra() {
        assert_eq!(solve_part_one(EXAMPLE_EXTRA), 1);
    }

    #[test]
    fn test_part_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 140);
    }
}
