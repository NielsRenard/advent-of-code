#![allow(dead_code, unused_mut, unused_variables)]
#[macro_use]
extern crate measure_time;


fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/1.input").unwrap()
}


fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {}", answer_1);
    println!("part 2: {}", answer_2);
    //execution took 0.1ms
}

fn solve_part_one(input: &str) -> i32 {
    let lines: Vec<i32> = input.lines().map(|s| s.parse::<i32>().unwrap()).collect();
    let mut increases: i32 = 0;
    lines.windows(2).for_each(|window| {
        if window[0] < window[1] {
            increases += 1
        }
    });
    increases
}

fn solve_part_two(input: &str) -> i32 {
    let lines: Vec<i32> = input.lines().map(|s| s.parse::<i32>().unwrap()).collect();
    let mut increases: i32 = 0;
    lines.windows(4).for_each(|window| {
        if window[0] + window[1] + window[2] < window[1] + window[2] + window[3] {
            increases += 1
        }
    });
    increases
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n";
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 7);
    }
    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 5);
    }
}
