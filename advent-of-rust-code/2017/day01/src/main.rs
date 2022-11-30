#![allow(dead_code, unused_mut, unused_variables)]
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2017/1.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {}", answer_1);
    println!("part 2: {}", answer_2);
}

fn char_to_int(c: char) -> i32 {
    c.to_digit(10).unwrap().try_into().unwrap()
}

fn solve_part_one(input: &str) -> i32 {
    let (first, last) = (
        input.chars().next().unwrap(),
        input.chars().nth_back(1).unwrap(),
    );

    let sum = input
        .lines()
        .next()
        .unwrap()
        .chars()
        .into_iter()
        .map(char_to_int)
        .collect::<Vec<i32>>()
        .windows(2)
        .map(|window| if window[0] == window[1] { window[0] } else { 0 })
        .sum();

    if char_to_int(first) == char_to_int(last) {
        println!("last{:?}", last);
        sum + char_to_int(first)
    } else {
        println!("last{:?}", last);
        sum
    }
}

fn solve_part_two(input: &str) -> i32 {
    let numbers = input
        .lines()
        .next()
        .unwrap()
        .chars()
        .into_iter()
        .map(char_to_int)
        .collect::<Vec<i32>>();
    let middle_of_list = numbers.len() / 2;
    let halfway_pairs: Vec<Vec<i32>> = numbers
        .iter()
        .enumerate()
        .map(|(i, n)| {
            let halfway = if i < middle_of_list {
                n + numbers[middle_of_list]
            } else {
                let till_end = numbers.len() - i;
                let wrapped_around = middle_of_list - till_end;
                numbers[wrapped_around]
            };
            vec![*n, halfway]
        })
        .collect::<Vec<Vec<i32>>>();

    // println!("{:?}", halfway_pairs);

    let sum: i32 = halfway_pairs
        .iter()
        .map(|v| if v[0] == v[1] { v[0] + v[1] } else { 0 })
        .sum();

    sum
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: [&str; 4] = ["1122\n", "1111\n", "1234\n", "91212129\n"];
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT[0]), 3);
        assert_eq!(solve_part_one(EXAMPLE_INPUT[1]), 4);
        assert_eq!(solve_part_one(EXAMPLE_INPUT[2]), 0);
        assert_eq!(solve_part_one(EXAMPLE_INPUT[3]), 9);
    }

    const EXAMPLE_INPUT_2: [&str; 5] = ["1212\n", "1221\n", "123425\n", "123123\n", "12131415\n"];
    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT_2[0]), 6);
        assert_eq!(solve_part_two(EXAMPLE_INPUT_2[1]), 0);
        assert_eq!(solve_part_two(EXAMPLE_INPUT_2[2]), 4);
        assert_eq!(solve_part_two(EXAMPLE_INPUT_2[3]), 12);
        assert_eq!(solve_part_two(EXAMPLE_INPUT_2[4]), 4);
    }
}
