#![allow(dead_code, unused_mut, unused_variables)]
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/2.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {}", answer_1);
    println!("part 2: {}", answer_2);
    // execution took 0.15ms
}

fn solve_part_one(input: &str) -> i64 {
    let mut depth = 0;
    let mut hor_pos = 0;

    input.lines().for_each(|s| {
        let mut split = s.split(" ");
        let direction = split.next().unwrap();
        let distance = split.next().unwrap().parse::<i64>().unwrap();
        match direction {
            "forward" => {
                hor_pos += distance;
            }
            "down" => {
                depth += distance;
            }
            "up" => {
                depth -= distance;
            }
            _ => {}
        }
    });

    depth * hor_pos
}

fn solve_part_two(input: &str) -> i64 {
    let mut velocity = 0;
    let mut depth = 0;
    let mut hor_pos = 0;

    input.lines().for_each(|s| {
        let mut split = s.split(" ");
        let command = split.next().unwrap();
        let value = split.next().unwrap().parse::<i64>().unwrap();

        match command {
            "forward" => {
                hor_pos += value;
                depth += velocity * value;
            }
            "down" => {
                velocity += value;
            }
            "up" => {
                velocity -= value;
            }
            _ => {}
        }
    });

    depth * hor_pos
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "forward 5
down 5
forward 8
up 3
down 8
forward 2
";
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 150);
    }
    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 900);
    }
}
