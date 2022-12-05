#![allow(dead_code, unused_mut, unused_variables)]

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/5.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {answer_1}");
    println!("part 2: {answer_2}");
}

fn solve_part_one(input: &str) -> String {
    let (mut stacks, moves) = parse_input(input);
    for mov in moves {
        do_move(&mut stacks, mov);
    }
    take_top_crates(&stacks)
}

fn solve_part_two(input: &str) -> String {
    let (mut stacks, moves) = parse_input(input);
    for mov in moves {
        do_move_part_two(&mut stacks, mov);
    }
    take_top_crates(&stacks)
}

fn do_move(stacks: &mut [Vec<char>], mov: Vec<usize>) {
    let count = mov[0];
    let from = mov[1] - 1;
    let to = mov[2] - 1;
    for _ in 0..count {
        let letter = stacks[from].pop().unwrap();
        stacks[to].push(letter);
    }
}

fn take_top_crates(stacks: &[Vec<char>]) -> String {
    stacks
        .iter()
        .map(|v| v.last().unwrap())
        .fold(String::new(), |a, b| a + &b.to_string())
}

fn do_move_part_two(stacks: &mut [Vec<char>], mov: Vec<usize>) {
    let count = mov[0];
    let from = mov[1] - 1;
    let to = mov[2] - 1;

    let mut to_move: Vec<char> = vec![];
    for _ in 0..count {
        let letter = stacks[from].pop().unwrap();
        to_move.push(letter);
    }
    for letter in to_move.into_iter().rev() {
        stacks[to].push(letter);
    }
}

/// Returns the initial crate configuration as a list of stacks
/// and the moves as a list of [how many, from, to]
fn parse_input(input: &str) -> (Vec<Vec<char>>, Vec<Vec<usize>>) {
    let line_length = input.lines().next().unwrap().len();
    let stack_count = line_length / 4 + 1;

    let mut stacks: Vec<Vec<char>> = vec![Vec::new(); stack_count];

    let (drawing, moves) = input.split_once("\n\n").unwrap();

    for l in drawing.lines().rev() {
        for (n, stack) in stacks.iter_mut().enumerate() {
            let character = l.chars().nth(n * 4 + 1).unwrap();
            if character.is_alphabetic() {
                stack.push(character)
            }
        }
    }

    let moves = moves
        .lines()
        .map(|l| {
            let parts: Vec<_> = l.splitn(6, ' ').collect();
            vec![
                parts[1].parse().unwrap(),
                parts[3].parse().unwrap(),
                parts[5].parse().unwrap(),
            ]
        })
        .collect();

    (stacks, moves)
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"#;

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), "CMZ".to_string());
    }
    #[test]
    fn test_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), "MCD".to_string());
    }
}
