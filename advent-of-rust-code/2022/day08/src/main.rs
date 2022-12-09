#![allow(dead_code, unused_mut, unused_variables)]

pub use std::collections::HashMap;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/8.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());

    println!("part 1: {answer_1}");
    println!("part 2: {answer_2}");
}

fn solve_part_one(input: &str) -> usize {
    let matrix = to_matrix(input);
    print_grid(&matrix);
    let visible_trees = visible_tree_coordinates(&matrix);
    // assuming the forest is square
    matrix.len() * 4 - 4 + visible_trees.len()
}

fn solve_part_two(input: &str) -> usize {
    let matrix = to_matrix(input);
    let visible_trees = visible_tree_coordinates(&matrix);
    let scenic_scores = scenic_scores(&matrix, &visible_trees);
    println!("{scenic_scores:?}");
    *scenic_scores.iter().max().unwrap()
}

fn visible_tree_coordinates(matrix: &[Vec<u32>]) -> Vec<(usize, usize)> {
    let mut visible_trees: Vec<(usize, usize)> = vec![];
    for (iy, row) in matrix.iter().enumerate() {
        if iy == 0 || iy == matrix.len() - 1 {
            // top or bottom edge
            continue;
        }
        for (ix, tree) in row.iter().enumerate() {
            if ix == 0 || ix == row.len() - 1 {
                // left or right edge
                continue;
            } else {
                let entire_column = matrix.iter().map(|row| row[ix]).collect::<Vec<u32>>();
                [
                    &row[..ix],
                    &row[ix + 1..],
                    &entire_column[..iy],
                    &entire_column[iy + 1..],
                ]
                .iter()
                .any(|direction| direction.iter().all(|height| height < tree))
                .then(|| visible_trees.push((ix, iy)));
            }
        }
    }
    visible_trees
}

fn scenic_scores(matrix: &[Vec<u32>], tree_coordinates: &[(usize, usize)]) -> Vec<usize> {
    let mut tree_scores: Vec<usize> = vec![];
    for &(x, y) in tree_coordinates {
        let tree_house = matrix[y][x];
        let entire_column = matrix.iter().map(|row| row[x]).collect::<Vec<u32>>();

        let mut left = matrix[y][..x].to_owned();
        let right = matrix[y][x + 1..].to_owned();
        let mut up = entire_column[..y].to_owned();
        let down = entire_column[y + 1..].to_owned();
        left.reverse();
        up.reverse();

        let directions = [left, right, up, down];

        let mut visible_trees = vec![vec![]; 4];
        for (i, view) in directions.iter().enumerate() {
            for &height in view.iter() {
                if height < tree_house {
                    visible_trees[i].push(height);
                } else if height >= tree_house {
                    visible_trees[i].push(height);
                    break;
                }
            }
        }

        let scenic_score = visible_trees
            .iter()
            .map(|trees| trees.len())
            .reduce(|a, b| a * b)
            .unwrap();
        tree_scores.push(scenic_score);
        println!("y: {y}, x {x}, height {tree_house}, {visible_trees:?}");
    }
    tree_scores
}

fn to_matrix(input: &str) -> Vec<Vec<u32>> {
    input
        .lines()
        .into_iter()
        .filter_map(|l| l.chars().map(|x| x.to_digit(10)).collect())
        .collect()
}

fn print_grid(matrix: &[Vec<u32>]) {
    for y in matrix {
        for x in y {
            print!("{x} ");
        }
        println!();
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"30373
25512
65332
33549
35390
"#;

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 21);
    }
    #[test]
    fn test_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 8);
    }
}
