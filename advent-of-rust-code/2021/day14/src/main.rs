#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment)]

use std::collections::HashMap;
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/13.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    let answer_2 = solve_part_two(&input_from_file());
}

#[derive(Debug)]
struct Cave {
    connected_caves: Vec<usize>,
    small: bool,
}

fn solve_part_one(input: &str) -> u32 {
    let (grid, fold_instructions): (Vec<Vec<u32>>, Vec<(char, usize)>) = parse(input);
    // pretty_print(&grid);
    let mut folded_grid = grid.clone();
    for (direction, fold_line) in fold_instructions.iter().nth(0) {
        match direction {
            'x' => {
                folded_grid = hacky_fold_left(&folded_grid, *fold_line);
                // println!();
                // println!("After fold_left");
                // pretty_print(&folded_grid);
            }
            'y' => {
                folded_grid = fold_up(&folded_grid, *fold_line);
                // println!();
                // println!("After fold_up");
                // pretty_print(&folded_grid);
            }
            _ => panic!(),
        }
    }
    folded_grid.iter().flatten().filter(|n| **n > 0).sum()
}

fn solve_part_two(input: &str) -> u32 {
    let (grid, fold_instructions): (Vec<Vec<u32>>, Vec<(char, usize)>) = parse(input);
    // println!("Starting input");
    // pretty_print(&grid);
    let mut folded_grid = grid.clone();
    for (direction, fold_line) in fold_instructions {
        match direction {
            'x' => {
                folded_grid = fold_left_using_rotations(&folded_grid, fold_line);
                // println!();
                // println!("After fold_left along x={}", fold_line);
                // pretty_print(&folded_grid);
            }
            'y' => {
                folded_grid = fold_up(&folded_grid, fold_line);
                // println!();
                // println!("After fold_up along y={}", fold_line);
                // pretty_print(&folded_grid);
            }
            _ => panic!(),
        }
    }
    pretty_print(&folded_grid);
    folded_grid.iter().flatten().filter(|n| **n > 0).sum()
}

fn fold_up(grid: &Vec<Vec<u32>>, y_line: usize) -> Vec<Vec<u32>> {
    let (above_ref, below_ref) = grid.split_at(y_line);
    let (mut above, mut below): (Vec<Vec<u32>>, Vec<Vec<u32>>) =
        (above_ref.to_vec().clone(), below_ref.to_vec().clone());
    let without_fold_line = Vec::from_iter(below[1..below.len()].iter().cloned());
    below = without_fold_line;
    below.reverse();

    let max_x = grid[0].len();
    let mut new_grid: Vec<Vec<u32>> = vec![vec![0; max_x]; below.len()];
    for y in 0..above.len() {
        for x in 0..max_x {
            if above[y][x] == 1 || below[y][x] == 1 {
                new_grid[y][x] = 1
            }
        }
    }
    new_grid
}

fn hacky_fold_left(grid: &Vec<Vec<u32>>, x_line: usize) -> Vec<Vec<u32>> {
    let left_halves: Vec<Vec<u32>> = grid
        .iter()
        .map(|row| Vec::from_iter(row[0..x_line].iter().cloned()))
        .collect();

    let right_halves: Vec<_> = grid
        .iter()
        .map(|row| {
            let max_x = row.len();
            let mut right_half = Vec::from_iter(row[x_line + 1..max_x].iter().cloned());
            right_half.reverse();
            right_half
        })
        .collect();
    let max_x = left_halves[0].len();
    let max_y = grid.len();
    let mut new_grid: Vec<Vec<u32>> = vec![vec![0; max_x]; max_y];
    for y in 0..grid.len() {
        for x in 0..left_halves[0].len() {
            if left_halves[y][x] == 1 || right_halves[y][x] == 1 {
                new_grid[y][x] = 1
            }
        }
    }
    new_grid
}

fn fold_left_using_rotations(grid: &Vec<Vec<u32>>, x_line: usize) -> Vec<Vec<u32>> {
    /*
    to rotate 90 degrees:
     1. transpose array
     2. reverse each row
    */
    // 1. transpose array (rows become columns)
    let max_x = grid[0].len();
    let max_y = grid.len();
    let mut transpose: Vec<Vec<u32>> = vec![vec![0; max_y]; max_x];
    for y in 0..max_y {
        for x in 0..max_x {
            transpose[x][y] = grid[y][x];
        }
    }
    // 2. reverse each row
    transpose.iter_mut().for_each(|r| r.reverse());

    let mut rotated = fold_up(&transpose, x_line);

    /*
    to rotate -90 degrees:
     1. transpose array
     2. reverse each column
    */
    // 1. transpose array (rows become columns)
    let mut rotated_back: Vec<Vec<u32>> = vec![vec![0; rotated.len()]; rotated[0].len()];
    for y in 0..rotated.len() {
        for x in 0..rotated[0].len() {
            rotated_back[x][y] = rotated[y][x];
        }
    }
    // 2. reverse each column
    rotated_back.reverse();
    rotated_back
}

fn parse(input: &str) -> (Vec<Vec<u32>>, Vec<(char, usize)>) {
    let dots: Vec<(usize, usize)> = input
        .lines()
        .into_iter()
        .take_while(|line| line.ne(&""))
        .map(|dot_line| dot_line.split_once(',').unwrap())
        .map(|(x, y)| (x.parse().unwrap(), y.parse().unwrap()))
        .collect();
    let folds: Vec<(char, usize)> = input
        .lines()
        .into_iter()
        .skip_while(|line| line.ne(&""))
        .skip(1)
        .map(|line| line.chars().into_iter().skip(11).collect::<String>())
        .map(|line| {
            (
                line.chars().next().unwrap(),
                line.chars().skip(2).collect::<String>(),
            )
        })
        .map(|(x, n)| (x, n.parse().unwrap()))
        .collect();

    // this is what was causing my bug!!! I assumed the highest x and y were the highest dots, ugh!
    // let max_x = dots.iter().max_by_key(|(x,_)| x).unwrap().0 as usize;
    // let max_y = dots.iter().max_by_key(|(_,y)| y).unwrap().1 as usize;

    // making my output look like this:
    
    // ..##.##.#.####.####.#....####.#.##.#..#.
    // ...#.####..##..####.#....#.##.###..####.
    // #..#.###..##...#..#.#....#.##.#.#..#..#.
    // ##.#.#.##.####.###..####.####.#.#..#..#.

    // determine max x and y based on folds
    let max_x = if folds[0].0 == 'x' {
        folds[0].1 * 2 + 1
    } else {
        folds[1].1 * 2 + 1
    };
    let max_y = if folds[0].0 == 'y' {
        folds[0].1 * 2 + 1
    } else {
        folds[1].1 * 2 + 1
    };


    
    let mut grid: Vec<Vec<u32>> = vec![vec![0; max_x]; max_y];
    dots.iter().for_each(|(x, y)| grid[*y][*x] = 1);
    (grid, folds)
}

fn pretty_print(grid: &Vec<Vec<u32>>) {
    println!("x:{}, y:{}", grid[0].len(), grid.len());
    let max_width = grid[0].len();
    for r in grid.iter() {
        for (i, n) in r.iter().enumerate() {
            if *n == 0 {
                print!(" ")
            } else {
                print!("#")
            }
            if i == max_width - 1 {
                println!();
            }
        }
    }
}
#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 17);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 999);
    }
}
