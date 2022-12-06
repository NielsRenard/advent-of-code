#![allow(dead_code, unused_mut, unused_variables)]

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/6.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {answer_1}");
    println!("part 2: {answer_2}");
}

fn solve_part_one(input: &str) -> u32 {
    let mut iter = input.as_bytes().windows(4);
    let mut count = 4;
    while let Some(&[a, b, c, d]) = iter.next() {
        if a == b || a == c || a == d || b == c || b == d || c == d {
            count += 1;
        } else {
            break;
        }
    }
    count
}

fn solve_part_two(input: &str) -> u32 {
    let mut iter = input.as_bytes().windows(14);

    let mut count = 14;
    for window in iter {
        let mut uniques = window.to_owned();
        uniques.sort();
        uniques.dedup();

        if uniques.len() == 14 {
            break;
        }
        count += 1;
    }
    count
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: [&str; 5] = [
        "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
        "bvwbjplbgvbhsrlpgdmjqwftvncz",
        "nppdvjthqldpwncqszvftbrmjlhg",
        "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
        "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw",
    ];

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT[0]), 7);
        assert_eq!(solve_part_one(EXAMPLE_INPUT[1]), 5);
        assert_eq!(solve_part_one(EXAMPLE_INPUT[2]), 6);
        assert_eq!(solve_part_one(EXAMPLE_INPUT[3]), 10);
        assert_eq!(solve_part_one(EXAMPLE_INPUT[4]), 11);
    }
    #[test]
    fn test_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT[0]), 19);
        assert_eq!(solve_part_two(EXAMPLE_INPUT[1]), 23);
        assert_eq!(solve_part_two(EXAMPLE_INPUT[2]), 23);
        assert_eq!(solve_part_two(EXAMPLE_INPUT[3]), 29);
        assert_eq!(solve_part_two(EXAMPLE_INPUT[4]), 26);
    }
}
