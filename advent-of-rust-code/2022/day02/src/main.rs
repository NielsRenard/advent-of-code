#![allow(dead_code, unused_mut, unused_variables)]
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/2.input").unwrap()
}

fn main() {
    print_time!("execution");
    let (answer_1, answer_2) = solve(&input_from_file());
    println!("part 1: {}", answer_1);
    println!("part 2: {}", answer_2);
}

fn solve(input: &str) -> (u32, u32) {
    let pairs = extract_pairs(input);
    (pairs.iter().map(calculate_score_part_1).sum::<u32>(),
     pairs.iter().map(calculate_score_part_2).sum::<u32>())
}

fn extract_pairs(input: &str) -> Vec<(char, char)> {
    input
        .lines()
        .map(|l| l.as_bytes())
        .map(|l| (l[0] as char, l[2] as char))
        .collect::<Vec<(char, char)>>()
}

/// Your score is for the shape you selected:
/// 1 for Rock (X),
/// 2 for Paper (Y),
/// 3 for Scissors (Z)
/// ...plus the score for the outcome of the round:
/// 0 if you lost,
/// 3 if the round was a draw,
/// 6 if you won.
fn calculate_score_part_1(pair: &(char, char)) -> u32 {
    match pair {
        ('A', 'X') => 1 + 3,
        ('A', 'Y') => 2 + 6,
        ('A', 'Z') => 3,

        ('B', 'X') => 1,
        ('B', 'Y') => 2 + 3,
        ('B', 'Z') => 3 + 6,

        ('C', 'X') => 1 + 6,
        ('C', 'Y') => 2,
        ('C', 'Z') => 3 + 3,
        _ => panic!("illegal characters found in pair"),
    }
}

///The Elf finishes helping with the tent and sneaks back over to
/// you. "Anyway, the second column says how the round needs to end: X
/// means you need to lose, Y means you need to end the round in a
/// draw, and Z means you need to win. Good luck!"
fn calculate_score_part_2(pair: &(char, char)) -> u32 {
    match pair {
        ('A', 'X') => 3,
        ('A', 'Y') => 1 + 3,
        ('A', 'Z') => 2 + 6,

        ('B', 'X') => 1,
        ('B', 'Y') => 2 + 3,
        ('B', 'Z') => 3 + 6,

        ('C', 'X') => 2,
        ('C', 'Y') => 3 + 3,
        ('C', 'Z') => 1 + 6,
        _ => panic!("illegal characters found in pair"),
    }
}


#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"A Y
B X
C Z"#;

    #[test]
    fn test() {
        assert_eq!(solve(EXAMPLE_INPUT), (15, 12));
    }
}
