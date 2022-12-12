#![allow(dead_code, clippy::type_complexity, unused_mut, unused_variables)]

use pathfinding::prelude::{bfs, Matrix};
pub use std::collections::HashMap;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/12.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {answer_1}");
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {answer_2}");
}

fn solve_part_one(input: &str) -> usize {
    let (matrix, start_pos, end_pos) = &make_matrix(input);
    let shortest_path = bfs(
        start_pos,
        |&(x, y)| {
            matrix
                .neighbours((x, y), false)
                .filter(move |&(p, q)| matrix[(p, q)] <= matrix[(x, y)] + 1)
        },
        |&p| p == *end_pos,
    )
    .expect("no path found");

    // minus 1 because shortest_path includes both the initial and
    // final position
    shortest_path.len() - 1
}

fn solve_part_two(input: &str) -> usize {
    let (matrix, _, end_pos) = &make_matrix(input);
    let shortest_path = bfs(
        end_pos,
        |&pos| {
            matrix
                .neighbours(pos, false)
                .filter(move |&new_pos| matrix[new_pos] + 1 >= matrix[pos])
        },
        |&p| matrix[p] == b'a',
    )
    .expect("no path found");

    // minus 1 because shortest_path includes both the initial and
    // final position
    shortest_path.len() - 1
}

#[deprecated]
fn solve_part_two_silly_brute_force(input: &str) -> usize {
    let (matrix, _, end_pos) = &make_matrix(input);
    let a_positions = matrix
        .indices()
        .filter(|&pos| matrix[pos] == b'a')
        .collect::<Vec<_>>();
    let mut paths: Vec<Vec<(usize, usize)>> = vec![];
    for start_pos in &a_positions {
        let path = bfs(
            start_pos,
            |&(x, y)| {
                matrix
                    .neighbours((x, y), false)
                    .filter(move |&(p, q)| matrix[(p, q)] <= matrix[(x, y)] + 1)
            },
            |&p| p == *end_pos,
        );
        if let Some(path) = path {
            paths.push(path)
        }
    }
    let shortest_path = paths.iter().map(|p| p.len()).min().unwrap();
    // minus 1 because shortest_path includes both the initial and
    // final position
    shortest_path - 1
}

/// Returns a height-map matrix and our start and ending positions
fn make_matrix(input: &str) -> (Matrix<u8>, (usize, usize), (usize, usize)) {
    let mut matrix = Matrix::from_rows(input.lines().map(|l| l.chars().map(|c| c as u8))).unwrap();
    let start_pos = matrix.indices().find(|&pos| matrix[pos] == b'S').unwrap();
    let end_pos = matrix.indices().find(|&pos| matrix[pos] == b'E').unwrap();

    matrix[start_pos] = b'a';
    matrix[end_pos] = b'z';
    (matrix, start_pos, end_pos)
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"#;

    /*
       0 1 2 3 4 5 6 7
    0  S a b q p o n m
    1  a b c r y x x l
    2  a c c s z E x k
    3  a c c t u v w j
    4  a b d e f g h i

    */

    #[test]
    fn test_one() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 31);
    }
    #[test]
    fn test_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 29);
    }
}
