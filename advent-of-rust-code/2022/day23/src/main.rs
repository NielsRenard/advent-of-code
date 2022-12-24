#![feature(result_option_inspect, string_extend_from_within)]
#![allow(dead_code, clippy::type_complexity)]

use std::ops::Not;

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/23.input").unwrap()
}

#[doc(hidden)]
fn main() {
    env_logger::init();
    print_time!("execution");
    let answer_1: usize = solve_part_1(&input_from_file());
    println!("part 1: {answer_1}");

    let answer_2: usize = solve_part_2(&input_from_file());
    println!("part 2: {answer_2}");
    // part 2: 1016
    // execution took 34s <---- lol. but no time to optimize, I have to start the next puzzle
}

const N: usize = 3;
const S: usize = 1;
const W: usize = 2;
const E: usize = 0;

// directions as day 22:
//             > v < ^
//             0 1 2 3
//             E S W N

fn solve_part_1(input: &str) -> usize {
    let mut elves: Vec<(usize, usize)> = parse_input(input);

    if log_enabled!(log::Level::Debug) {
        pretty_print(&elves, 5);
    };

    let mut direction = [N, S, W, E];
    for n in 1..=10 {
        debug!("Round {n}");
        let finished_moving = run_round(&mut elves, &mut direction);
        if log_enabled!(log::Level::Debug) {
            pretty_print(&elves, 5);
        };
        if finished_moving {
            debug!("Elves are done moving around after {n} rounds");
            break;
        }
    }
    count_empty_tiles(&elves)
}

fn solve_part_2(input: &str) -> usize {
    let mut elves: Vec<(usize, usize)> = parse_input(input);

    let mut direction = [N, S, W, E];
    let mut round = 1;
    loop {
        info!("Round {round}");
        let finished_moving = run_round(&mut elves, &mut direction);
        if log_enabled!(log::Level::Info) {
            pretty_print(&elves, 5);
        };
        if finished_moving {
            debug!("Elves are done moving around after {round} rounds");
            break;
        }
        round += 1;
    }
    round
}

fn count_empty_tiles(coords: &[(usize, usize)]) -> usize {
    let x_min = coords.iter().map(|(x, _)| x).min().unwrap();
    let x_max = coords.iter().map(|(x, _)| x).max().unwrap();

    let y_min = coords.iter().map(|(_, y)| y).min().unwrap();
    let y_max = coords.iter().map(|(_, y)| y).max().unwrap();

    let x_width = (*x_max - *x_min) + 1;
    let y_height = (*y_max - *y_min) + 1;
    println!("x_width->{x_width}, y_height->{y_height}");
    let mut rectangle: Vec<Vec<char>> = vec![vec!['.'; x_width]; y_height];

    for y in *y_min..=*y_max {
        for x in *x_min..=*x_max {
            if coords.contains(&(x, y)) {
                rectangle[y - y_min][x - x_min] = '#';
            }
        }
    }

    // print the cropped rectangle
    println!();
    (0..x_width).for_each(|n| print!("{n}"));
    println!();
    for (i, row) in rectangle.iter().enumerate() {
        for col in row {
            print!("{col}");
        }
        println!(" {i}");
    }
    println!();

    rectangle.iter().flatten().filter(|&&c| c == '.').count()
}

//             0 1 2 3
//             E S W N

/// returns true if elves finished moving
fn run_round(elves: &mut [(usize, usize)], direction: &mut [usize; 4]) -> bool {
    // first half of the round: proposing
    let mut finished_moving = 0;

    let mut proposed_pos: Vec<(usize, usize)> = vec![];
    for pos in elves.iter() {
        let adj = get_eight_adjacent_coordinates(pos);

        let nobody_adjacent = adj
            .iter()
            .any(|(adj_x, adj_y)| elves.contains(&(*adj_x, *adj_y)))
            .not();

        if nobody_adjacent {
            debug!("nobody adjacent, relaxing...");
            // this elf has nobody around, and therefore won't act this round
            finished_moving += 1;
            proposed_pos.push(*pos);
            continue;
        }

        let mut proposed = false;
        debug!("Elf {pos:?} dir: {direction:?}");
        for n in 0..=3 {
            if !proposed {
                let next_dir = direction[n];
                match next_dir {
                    0 => {
                        // E
                        if nobody_e_ne_se(pos, elves) {
                            debug!("nobody there, let's propose East!");
                            proposed_pos.push((pos.0 + 1, pos.1));
                            proposed = true;
                            break;
                        }
                    }
                    1 => {
                        // S
                        if nobody_s_se_sw(pos, elves) {
                            debug!("nobody there, let's propose South!");
                            proposed_pos.push((pos.0, pos.1 + 1));
                            proposed = true;
                            break;
                        }
                    }
                    2 => {
                        // W
                        if nobody_w_nw_sw(pos, elves) {
                            debug!("nobody there, let's propose West!");
                            proposed_pos.push((pos.0 - 1, pos.1));
                            proposed = true;
                            break;
                        }
                    }
                    3 => {
                        // N
                        if nobody_n_ne_nw(pos, elves) {
                            debug!("nobody there, let's propose North!");
                            proposed_pos.push((pos.0, pos.1 - 1));
                            proposed = true;
                            break;
                        }
                    }
                    _ => panic!(),
                }
            }
        }
        if !proposed {
            proposed_pos.push(*pos)
        }
    }

    // second half of the round: possibly acting
    for (i, cur_pos) in elves.iter_mut().enumerate() {
        let prop_pos: (usize, usize) = proposed_pos[i];
        if proposed_pos.iter().filter(|&&p| p == prop_pos).count() >= 2 {
            // multiple elves proposed, don't act
            debug!("Elf: {cur_pos:?} too crowded, did not move...")
        } else {
            debug!("Elf: {cur_pos:?} moved to {:?}!", prop_pos);
            *cur_pos = prop_pos;
        }
    }

    // the first direction the Elves considered is moved to the end of the list of directions
    direction.rotate_left(1);
    finished_moving == elves.len()
}

fn get_eight_adjacent_coordinates((x, y): &(usize, usize)) -> Vec<(usize, usize)> {
    vec![
        //  left, right, top, and bottom
        (x - 1, *y), // W
        (x + 1, *y), // E
        (*x, y - 1), // N
        (*x, y + 1), // S
        // top-left, top-right, bottom-left, and bottom-right
        (x - 1, y - 1), // NW
        (x + 1, y - 1), // NE
        (x - 1, y + 1), // SW
        (x + 1, y + 1), // SE
    ]
}

fn nobody_n_ne_nw((x, y): &(usize, usize), elves: &[(usize, usize)]) -> bool {
    (vec![
        (x + 1, y - 1), // NE
        (*x, y - 1),    //  N
        (x - 1, y - 1), // NW
    ])
    .iter()
    .any(|(x, y)| elves.contains(&(*x, *y)))
    .not()
}

fn nobody_s_se_sw((x, y): &(usize, usize), elves: &[(usize, usize)]) -> bool {
    (vec![
        (*x, y + 1),    // S
        (x + 1, y + 1), // SE
        (x - 1, y + 1), // SW
    ])
    .iter()
    .any(|(x, y)| elves.contains(&(*x, *y)))
    .not()
}

fn nobody_w_nw_sw((x, y): &(usize, usize), elves: &[(usize, usize)]) -> bool {
    (vec![
        (x - 1, y - 1), // NW
        (x - 1, *y),    // W
        (x - 1, y + 1), // SW
    ])
    .iter()
    .any(|(x, y)| elves.contains(&(*x, *y)))
    .not()
}

fn nobody_e_ne_se((x, y): &(usize, usize), elves: &[(usize, usize)]) -> bool {
    (vec![
        (x + 1, *y),    // E
        (x + 1, y - 1), // NE
        (x + 1, y + 1), // SE
    ])
    .iter()
    .any(|(x, y)| elves.contains(&(*x, *y)))
    .not()
}

fn parse_input(input: &str) -> Vec<(usize, usize)> {
    let mut elves: Vec<(usize, usize)> = vec![];

    input.lines().enumerate().for_each(|(i_y, line)| {
        line.chars().enumerate().for_each(|(i_x, c)| {
            if let '#' = c {
                // + so don't have to deal with 0's etc
                elves.push((i_x + 5, i_y + 5));
            }
        })
    });
    elves
}

fn pretty_print(coords: &[(usize, usize)], pad: usize) {
    let x_max = coords.iter().map(|(x, _)| x).max().unwrap() + pad;
    let y_max = coords.iter().map(|(y, _)| y).max().unwrap() + pad;

    for y in 0..y_max {
        let mut debug_str = String::new();
        for x in 0..x_max {
            if coords.contains(&(x, y)) {
                debug_str.push('#');
            } else {
                debug_str.push('.');
            }
        }
        debug_str.push_str(&format!(" | {y}"));
        debug!("{debug_str}");
    }
    debug!("0123456789111111");
    debug!("          012345");
}

#[cfg(test)]
mod tests {

    use super::*;

    const SMALL_EXAMPLE: &str = r#".....
..##.
..#..
.....
..##.
.....
"#;

    const LARGE_EXAMPLE: &str = r#"....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"#;

    const EXTRA_INPUT: &str = r#"..#....####...#.....###.#.#.#..#
.#.#.#..#...#..##..#..#...#....#
..###..##.###.#..##..#####..#..#
#..#.#..#.##..####..#..###..####
.#.###.#.##...#.....#...#.##.##.
.#.##..#..##.##...#.##..##.###..
.#####.#.....#...#.#.#........##
##.##...###..##.#.####..#..#.##.
.....#.##..#.##.#....##.##.#.#.#
#.###.#.###......#.#...#.##.#.##
#.#....###.#..#.#..####..#.#..##
##.#..###.....#.#.#....##.....#.
#...###..#.#..#####.....#......#
.##.###.....###.#..#.#.#..#...#.
.####.###.#..#.#.####......##.#.
#......##.####.#..#.#.##.#...#..
...##..#....#.#..#.#.#.#.###.###
#.....#.###.#.#..#...#.###..#...
...####.##..##.#......##..#...##
..##.##.##..###.#..#...#..#.#.#.
#....#..##.......##.#.#.##.#...#
#.#####....###.#.#..####.#.#.#.#
##.##.###.##.#.##.......####...#
###.#..#.###......###..#..###.#.
..##.##.##.#####.#####..###.#.#.
.####.#....#.##..###.#.##.#.##.#
#....##.###...###..#...#..#.##.#
.#..##..####..###.##.#####.##...
....#.###.#####.#.#..###....#..#
#..#....##..#.#..######.##...###
###.#.##........#.#.#.###.#.####
####.#..##.##..#.###.#.####.....
"#;

    #[test]
    pub fn test_one() {
        // env_logger::init();
        assert_eq!(solve_part_1(SMALL_EXAMPLE), 25);
        assert_eq!(solve_part_1(LARGE_EXAMPLE), 110);
    }

    #[test]
    pub fn test_two() {
        assert_eq!(solve_part_2(LARGE_EXAMPLE), 20);
    }

    #[test]
    pub fn test_extra() {
        assert_eq!(solve_part_1(EXTRA_INPUT), 1062);
    }
}
