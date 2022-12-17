#![allow(dead_code, unused_mut, unused_variables)]
#![feature(result_option_inspect, string_extend_from_within)]

use log::{debug, Level};
use std::{cmp::Ordering, collections::VecDeque};

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/17.input").unwrap()
}

#[doc(hidden)]
fn main() {
    env_logger::init();
    print_time!("execution");
    let answer_1 = solve(&input_from_file(), 2022);
    println!("part 1: {answer_1}");
}

#[derive(Debug)]
pub enum Jet {
    Left,
    Right,
}

impl From<Option<char>> for Jet {
    fn from(c: Option<char>) -> Self {
        match c.unwrap() {
            '<' => Self::Left,
            '>' => Self::Right,
            huh => panic!("Unknown jet: {huh}"),
        }
    }
}

pub fn solve(input: &str, number_of_rocks: usize) -> usize {
    let rock_list = rocks();

    let mut jets = input.lines().next().unwrap().chars().cycle();
    let mut rocks = rock_list.iter().cycle();
    let mut chamber: VecDeque<[char; 7]> = vec![].into();
    (1..=number_of_rocks).for_each(|n| {
        debug!("rock {n} begins to fall");
        let rock = rocks.next().unwrap();
        new_rock(&mut chamber, rock);
        if log_enabled!(Level::Debug) {
            pretty_print(&chamber);
        }
        let mut falling = true;

        while falling {
            let jet: Jet = jets.next().into();
            if moved_by_jet(&mut chamber, &jet) {
                debug!("Jet of gas pushes rock {jet:?}:");
                if log_enabled!(Level::Debug) {
                    pretty_print(&chamber);
                }
            } else {
                debug!("Jet of gas pushes rock {jet:?}, but nothing happens.");
                if log_enabled!(Level::Debug) {
                    pretty_print(&chamber);
                }
            }
            if moved_by_gravity(&mut chamber) {
                debug!("Rock falls 1 unit:");
                if log_enabled!(Level::Debug) {
                    pretty_print(&chamber);
                }
            } else {
                falling = false;
            }
        }
        debug!("Rock {n} falls 1 unit, causing it to come to rest:");
        if log_enabled!(Level::Debug) {
            pretty_print(&chamber);
        }
    });

    get_tower_height(&chamber)
}

fn get_tower_height(chamber: &VecDeque<[char; 7]>) -> usize {
    if let Some(height) = chamber
        .iter()
        .rev()
        .enumerate()
        .filter(|(i, row)| row.contains(&'#'))
        .last()
        .map(|(i, _)| i)
    {
        height + 1
    } else {
        0
    }
}

fn moved_by_gravity(chamber: &mut VecDeque<[char; 7]>) -> bool {
    let mut moved = false;
    let mut falling_rock: Vec<(usize, usize)> = vec![];
    for (i_y, row) in chamber.iter().enumerate() {
        for (i_x, col) in row.iter().enumerate() {
            if *col == '@' {
                falling_rock.push((i_x, i_y));
            }
        }
    }

    let collision = falling_rock.iter().any(|&(x, y)| {
        y == chamber.len() - 1 || (chamber[y + 1][x] != '.' && chamber[y + 1][x] != '@')
    });

    if collision {
        falling_rock.iter().for_each(|&(x, y)| {
            chamber[y][x] = '#';
        });
    } else {
        // clear old position
        falling_rock.iter().for_each(|(x, y)| {
            chamber[*y][*x] = '.';
        });

        // calculate new position
        falling_rock
            .iter_mut()
            .for_each(|mut coord| *coord = (coord.0, coord.1 + 1));

        // update chamber
        falling_rock.iter().for_each(|&(x, y)| {
            chamber[y][x] = '@';
        });
        moved = true;
    }
    moved
}

fn moved_by_jet(chamber: &mut VecDeque<[char; 7]>, jet: &Jet) -> bool {
    let mut falling_rock: Vec<(usize, usize)> = vec![];
    for (i_y, row) in chamber.iter().enumerate() {
        for (i_x, col) in row.iter().enumerate() {
            if *col == '@' {
                falling_rock.push((i_x, i_y));
            }
        }
    }
    let mut moved = false;
    match jet {
        Jet::Left => {
            if !falling_rock
                .iter()
                .any(|(x, y)| *x == 0 || (chamber[*y][x - 1] == '#'))
            {
                // delete original positions from chamber
                falling_rock.iter().for_each(|&(x, y)| {
                    chamber[y][x] = '.';
                });
                // transpose to the left
                falling_rock
                    .iter_mut()
                    .for_each(|mut coord| *coord = (coord.0 - 1, coord.1));
                moved = true;
            }
        }
        Jet::Right => {
            if !falling_rock
                .iter()
                .any(|(x, y)| *x == 6 || (chamber[*y][x + 1] == '#'))
            {
                // delete original positions from chamber
                falling_rock.iter().for_each(|&(x, y)| {
                    chamber[y][x] = '.';
                });
                // transpose to the right
                falling_rock
                    .iter_mut()
                    .for_each(|mut coord| *coord = (coord.0 + 1, coord.1));
                moved = true;
            }
        }
    };

    if moved {
        // insert new positions
        falling_rock.iter().for_each(|&(x, y)| {
            chamber[y][x] = '@';
        });
    }
    moved
}

pub fn new_rock(chamber: &mut VecDeque<[char; 7]>, rock: &Rock) {
    let tower_height = get_tower_height(chamber);

    //
    //           |...@...|0
    //           |..@@@..|1
    //           |...@...|2
    // |.......|0|.......|3
    // |.......|1|.......|4
    // |.......|2|.......|5
    // |..####.|3|..####.|6
    // +-------+ +-------+

    // x = 2 left of wall,
    // y = leave 3 empty rows above tower
    let offset_x = 2;
    let offset_y = tower_height + 4;

    // make sure new rock has a place to spawn
    // by extending or shrinking our chamber
    let need = tower_height + 3 + rock.height;
    let difference = usize::abs_diff(chamber.len(), need);
    debug!("need: {need} difference: {difference}");
    match need.cmp(&chamber.len()) {
        Ordering::Greater => {
            (0..difference).for_each(|_| {
                let empty_row = ['.'; 7];
                chamber.push_front(empty_row);
            });
        }
        Ordering::Less => {
            (0..difference).for_each(|_| {
                chamber.pop_front();
            });
        }
        Ordering::Equal => {}
    }

    for (x, y) in &rock.coords {
        chamber[*y][offset_x + x] = '@';
    }
}

/// a Rock is 4- or 5-polyomino  
/// `                    #`  
/// `####   #     #  ##  #`  
/// `      ###    #  ##  #`  
/// `       #   ###      #`  
#[derive(Clone)]
pub struct Rock {
    coords: Vec<(usize, usize)>,
    height: usize,
    left_most: (usize, usize),
    bottom: (usize, usize),
    right_most: (usize, usize),
}

pub fn rocks() -> Vec<Rock> {
    let mut hero = Rock {
        coords: vec![(0, 0), (1, 0), (2, 0), (3, 0)],
        height: 0,
        left_most: (0, 0),
        bottom: (0, 0),
        right_most: (0, 3),
    };

    let mut plus = Rock {
        coords: vec![(1, 0), (0, 1), (1, 1), (1, 2), (2, 1)],
        height: 0,
        left_most: (0, 0),
        bottom: (0, 0),
        right_most: (0, 3),
    };

    let mut blue_ricky = Rock {
        coords: vec![(2, 0), (2, 1), (0, 2), (1, 2), (2, 2)],
        height: 0,
        left_most: (0, 0),
        bottom: (0, 0),
        right_most: (0, 3),
    };

    let mut straight = Rock {
        coords: vec![(0, 0), (0, 1), (0, 2), (0, 3)],
        height: 0,
        left_most: (0, 0),
        bottom: (0, 0),
        right_most: (0, 3),
    };

    let mut smashboy = Rock {
        coords: vec![(0, 0), (1, 0), (0, 1), (1, 1)],
        height: 0,
        left_most: (0, 0),
        bottom: (0, 0),
        right_most: (0, 3),
    };

    hero.height = rock_height(&hero);
    plus.height = rock_height(&plus);
    blue_ricky.height = rock_height(&blue_ricky);
    straight.height = rock_height(&straight);
    smashboy.height = rock_height(&smashboy);

    vec![hero, plus, blue_ricky, straight, smashboy]
}

fn rock_height(rock: &Rock) -> usize {
    rock.coords
        .iter()
        .max_by(|(_, y_a), (_, y_b)| y_a.cmp(y_b))
        .unwrap()
        .1
        + 1
}

pub fn pretty_print(chamber: &VecDeque<[char; 7]>) {
    for (i, row) in chamber.iter().enumerate() {
        let mut row_string = String::new();
        row_string.push('|');
        for &col in row {
            row_string.push(col);
        }
        row_string.push_str(&format!("|{i}"));
        debug!("{row_string}");
    }
    debug!("+-------+");
    debug!("");
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"#;

    #[test]
    pub fn test() {
        env_logger::init();
        assert_eq!(solve(EXAMPLE_INPUT, 2022), 3068);
    }

    #[test]
    pub fn test_part_two() {
        env_logger::init();
        assert_eq!(solve(EXAMPLE_INPUT, 1_000_000_000_000), 1514285714288);
    }
}
