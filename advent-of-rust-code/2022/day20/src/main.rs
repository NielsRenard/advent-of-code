#![feature(result_option_inspect)]
#![allow(dead_code)]

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/20.input").unwrap()
}

#[doc(hidden)]
fn main() {
    print_time!("execution");
    let answer_1 = solve_part_1(&input_from_file());
    println!("part 1: {answer_1}");
    let answer_2 = solve_part_2(&input_from_file());
    println!("part 2: {answer_2}");
}

fn parse_input(input: &str) -> Vec<(usize, isize)> {
    input
        .lines()
        .enumerate()
        .map(|(i, n)| (i, n.parse::<isize>().unwrap()))
        .collect()
}

fn get_grove_coordinates(mixed: &[(usize, isize)]) -> isize {
    mixed
        .iter()
        .map(|(_, n)| n)
        .cycle()
        .skip_while(|&&n| n != 0)
        .step_by(1000)
        .take(4)
        .inspect(|x| println!("got {x}"))
        .sum::<isize>()
}

fn solve_part_1(input: &str) -> isize {
    get_grove_coordinates(&mix(&parse_input(input)))
}

fn solve_part_2(input: &str) -> isize {
    let list = parse_input(input)
        .iter()
        .map(|(i, n)| (*i, n * 811589153))
        .collect();
    // here's what I think of that
    let mixed = mix(&mix(&mix(&mix(&mix(&mix(&mix(&mix(&mix(&mix(&list))))))))));
    get_grove_coordinates(&mixed)
}

fn mix(list: &Vec<(usize, isize)>) -> Vec<(usize, isize)> {
    let mut mixed = list.clone();
    for i in 0..list.len() {
        let item @ (idx, n) = list.iter().find(|(idx, _)| *idx == i).unwrap();
        let cur_pos = mixed.iter().position(|x| x == item).unwrap() as isize;
        let new_pos = (cur_pos + n).rem_euclid((list.len() as isize) - 1);
        mixed.remove(cur_pos as usize);
        mixed.insert((new_pos) as usize, (*idx, *n));
    }

    mixed
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"1
2
-3
3
-2
0
4
"#;

    #[test]
    pub fn test() {
        assert_eq!(solve_part_1(EXAMPLE_INPUT), 3);
    }

    #[test]
    pub fn test_two() {
        assert_eq!(solve_part_2(EXAMPLE_INPUT), 1623178306);
    }
}
