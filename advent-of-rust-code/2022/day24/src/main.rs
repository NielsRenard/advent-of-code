#![feature(result_option_inspect, string_extend_from_within)]
#![allow(dead_code, clippy::type_complexity)]

use fxhash::FxHashMap;
use std::collections::{HashSet, VecDeque};

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/24.input").unwrap()
}

#[doc(hidden)]
fn main() {
    env_logger::init();
    print_time!("execution");
    let answer_1: usize = solve_part_1(&input_from_file());
    println!("part 1: {answer_1}");
    println!("part two:");
    let answer_2: usize = solve_part_2(&input_from_file());
    println!("part 2: {answer_2}");
}

#[derive(Debug, Clone)]
enum Tile {
    Empty,
    Player,
    Wall,
}

#[derive(Debug, Clone, Copy)]
enum Blizzard {
    Up(usize, usize),
    Down(usize, usize),
    Left(usize, usize),
    Right(usize, usize),
}

#[derive(Debug)]
struct Valley {
    tiles: Vec<Vec<Tile>>,
    blizzards: Vec<Blizzard>,
    height: usize,
    width: usize,
}

impl Valley {
    fn not_wall(&self, (x, y): (usize, usize)) -> bool {
        if y == self.height {
            return true;
        };
        !matches!(self.tiles[y][x], Tile::Wall)
    }

    fn blizzard_at(&self, (x, y): (usize, usize)) -> bool {
        for blizzard in &self.blizzards {
            match blizzard {
                Blizzard::Up(bx, by) => {
                    if (x, y) == (*bx, *by) {
                        return true;
                    }
                }
                Blizzard::Down(bx, by) => {
                    if (x, y) == (*bx, *by) {
                        return true;
                    }
                }
                Blizzard::Left(bx, by) => {
                    if (x, y) == (*bx, *by) {
                        return true;
                    }
                }
                Blizzard::Right(bx, by) => {
                    if (x, y) == (*bx, *by) {
                        return true;
                    }
                }
            }
        }
        false
    }

    fn advance_blizzards(&mut self) {
        for blizzard in self.blizzards.iter_mut() {
            match blizzard {
                Blizzard::Up(x, 1) => *blizzard = Blizzard::Up(*x, self.height - 2),
                Blizzard::Up(x, y) => *blizzard = Blizzard::Up(*x, *y - 1),
                Blizzard::Down(x, y) if *y == self.height - 2 => *blizzard = Blizzard::Down(*x, 1),
                Blizzard::Down(x, y) => *blizzard = Blizzard::Down(*x, *y + 1),
                Blizzard::Right(x, y) if *x == self.width - 2 => *blizzard = Blizzard::Right(1, *y),
                Blizzard::Right(x, y) => *blizzard = Blizzard::Right(*x + 1, *y),
                Blizzard::Left(1, y) => *blizzard = Blizzard::Left(self.width - 2, *y),
                Blizzard::Left(x, y) => *blizzard = Blizzard::Left(*x - 1, *y),
            }
        }
    }

    fn move_player(&mut self, (x, y): (usize, usize)) {
        for rows in self.tiles.iter_mut() {
            for tile in rows.iter_mut() {
                if matches!(tile, Tile::Player) {
                    *tile = Tile::Empty;
                }
            }
        }
        self.tiles[y][x] = Tile::Player;
    }
}

fn solve_part_1(input: &str) -> usize {
    let mut valley: Valley = parse_input(input);
    valley.tiles[0][1] = Tile::Player;
    println!();

    let start_pos = (1, 0);
    let end_pos = (valley.tiles[0].len() - 2, valley.tiles.len() - 1);
    let minutes: usize = 0;

    let least_steps = bfs(valley, start_pos, end_pos, minutes);

    println!("least_steps: {least_steps}");
    least_steps
}

fn solve_part_2(input: &str) -> usize {
    let mut valley: Valley = parse_input(input);
    valley.tiles[0][1] = Tile::Player;
    println!();

    let start_pos = (1, 0);
    let end_pos = (valley.tiles[0].len() - 2, valley.tiles.len() - 1);
    let minutes: usize = 0;

    let (valley_after_reaching_goal, minutes_1) =
        bfs_returning(valley, start_pos, end_pos, minutes);
    println!("1 down");
    let (and_after_back_to_start_again, minutes_2) = bfs_returning(
        valley_after_reaching_goal,
        (150, 21),
        start_pos,
        minutes_1 + 1,
    );
    println!("2 down");
    let (and_finally_back_to_goal_again, minutes_3) = bfs_returning(
        and_after_back_to_start_again,
        start_pos,
        end_pos,
        minutes_2 + 1,
    );
    println!("3 down: total time: {minutes_3}");

    minutes_3
}

type Pos = (usize, usize);

fn bfs(mut valley: Valley, start @ (_x, _y): Pos, goal @ (_gx, _gy): Pos, minutes: usize) -> usize {
    // store the time and the starting positition
    let mut to_visit: VecDeque<(usize, Pos)> = VecDeque::new();
    let mut visited: HashSet<(usize, Pos)> = HashSet::new();
    to_visit.push_back((minutes, start));
    visited.insert((minutes, start));

    // minutes to blizzards cache (we could also compute the
    // blizzards, since they are predictable)
    let mut cache: FxHashMap<usize, Vec<Blizzard>> = FxHashMap::default();
    cache.insert(minutes, valley.blizzards.clone());

    let mut least_steps = usize::MAX;

    while let Some((minutes, pos)) = to_visit.pop_front() {
        debug!("pop! minutes = {minutes} pos = {pos:?}");
        if pos == goal {
            println!("reached goal at {minutes} minutes, checking against heuristic");
            least_steps = least_steps.min(minutes);
            break;
        }

        let new_minutes = minutes + 1;

        if let Some(blizzards) = cache.get(&new_minutes) {
            valley.blizzards = blizzards.clone();
        } else {
            valley.advance_blizzards();
            cache.insert(new_minutes, valley.blizzards.clone());
        }

        valley.move_player(pos);

        // figure out which next steps are valid
        let nesw = get_adjacent_coordinates(&pos, valley.height, valley.width);
        for next_pos in nesw {
            if valley.not_wall(next_pos)
                && !visited.contains(&(new_minutes, next_pos))
                && !valley.blizzard_at(next_pos)
            {
                visited.insert((new_minutes, next_pos));
                to_visit.push_back((new_minutes, next_pos));
            } else {
                debug!("no good moves");
            }
        }
    }

    least_steps
}

// returns the valley state and the minutes
fn bfs_returning(
    mut valley: Valley,
    start @ (_x, _y): Pos,
    goal @ (_gx, _gy): Pos,
    minutes: usize,
) -> (Valley, usize) {
    // store the time and the starting positition
    let mut to_visit: VecDeque<(usize, Pos)> = VecDeque::new();
    let mut visited: HashSet<(usize, Pos)> = HashSet::new();
    to_visit.push_back((minutes, start));
    visited.insert((minutes, start));

    // minutes to blizzards cache (we could also compute the
    // blizzards, since they are predictable)
    let mut cache: FxHashMap<usize, Vec<Blizzard>> = FxHashMap::default();
    cache.insert(minutes, valley.blizzards.clone());

    let mut least_steps = usize::MAX;

    while let Some((minutes, pos)) = to_visit.pop_front() {
        if pos == goal {
            println!("reached goal at {minutes} minutes, checking against heuristic");
            least_steps = least_steps.min(minutes);
            to_visit.clear();
            return (valley, minutes);
            // continue;
        }

        let new_minutes = minutes + 1;

        if let Some(blizzards) = cache.get(&new_minutes) {
            valley.blizzards = blizzards.clone();
        } else {
            valley.advance_blizzards();
            cache.insert(new_minutes, valley.blizzards.clone());
        }

        valley.move_player(pos);
        // figure out which next steps are valid
        let nesw = get_adjacent_coordinates(&pos, valley.height, valley.width);
        for next_pos in nesw {
            let blizz_ok = !valley.blizzard_at(next_pos);
            let wall_ok = valley.not_wall(next_pos);
            if valley.not_wall(next_pos)
                && !visited.contains(&(new_minutes, next_pos))
                && !valley.blizzard_at(next_pos)
            {
                visited.insert((new_minutes, next_pos));
                to_visit.push_back((new_minutes, next_pos));
            } else {
                debug!("no good moves");
            }
        }
    }

    panic!()
}

fn get_adjacent_coordinates(
    (x, y): &(usize, usize),
    y_max: usize,
    x_max: usize,
) -> Vec<(usize, usize)> {
    if (*x, *y) == (1, 0) {
        // can only wait or move south from starting position
        return vec![(*x, y + 1), (*x, *y)];
    } else if *y == y_max - 1 {
        // can only go up when going from goal (just for part 2)
        return vec![(*x, y - 1), (*x, *y)];
    }
    vec![
        //  left, right, top, and bottom
        (*x, y - 1), // N
        (x + 1, *y), // E
        (*x, y + 1), // S
        (x - 1, *y), // W
        (*x, *y),    // CURRENT
    ]
}

fn parse_input(input: &str) -> Valley {
    let width = input.lines().next().unwrap().len();
    let height = input.lines().count();
    info!("{width} {height}");
    let mut tiles: Vec<Vec<Tile>> = vec![vec![Tile::Empty; width]; height];
    let mut blizzards: Vec<Blizzard> = vec![];
    for (iy, line) in input.lines().enumerate() {
        for (ix, tile) in line.chars().enumerate() {
            match tile {
                '#' => tiles[iy][ix] = Tile::Wall,
                '.' => tiles[iy][ix] = Tile::Empty,
                '<' => blizzards.push(Blizzard::Left(ix, iy)),
                '>' => blizzards.push(Blizzard::Right(ix, iy)),
                '^' => blizzards.push(Blizzard::Up(ix, iy)),
                'v' => blizzards.push(Blizzard::Down(ix, iy)),
                _ => panic!(),
            }
        }
    }
    Valley {
        tiles,
        blizzards,
        height,
        width,
    }
}

fn print_valley(valley: &Valley) {
    let mut blizzard_coords: Vec<((usize, usize), char)> = vec![];
    for blizzard in &valley.blizzards {
        match blizzard {
            Blizzard::Up(x, y) => blizzard_coords.push(((*x, *y), '^')),
            Blizzard::Down(x, y) => blizzard_coords.push(((*x, *y), 'v')),
            Blizzard::Left(x, y) => blizzard_coords.push(((*x, *y), '<')),
            Blizzard::Right(x, y) => blizzard_coords.push(((*x, *y), '>')),
        }
    }

    for (iy, row) in valley.tiles.iter().enumerate() {
        for (ix, tile) in row.iter().enumerate() {
            match tile {
                Tile::Empty => {
                    let blizzards: Vec<((usize, usize), char)> = blizzard_coords
                        .iter()
                        .filter(|((x, y), c)| (x, y) == (&ix, &iy))
                        .cloned()
                        .collect();
                    if !blizzards.is_empty() {
                        if blizzards.len() == 1 {
                            print!("{}", blizzards[0].1);
                        } else {
                            print!("{}", blizzards.len());
                        }
                    } else {
                        print!(".")
                    };
                }
                Tile::Player => print!("E"),
                Tile::Wall => print!("#"),
            }
        }
        println!();
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    const EASY_EXAMPLE: &str = r#"#.#####
#.....#
#>....#
#.....#
#...v.#
#.....#
#####.#
"#;

    const EXAMPLE_INPUT: &str = r#"#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"#;

    #[test]
    pub fn test_one() {
        // env_logger::init();
        // assert_eq!(solve_part_1(EASY_EXAMPLE), 18);
        assert_eq!(solve_part_1(EXAMPLE_INPUT), 18);
    }
    #[test]
    pub fn test_two() {
        // env_logger::init();
        // assert_eq!(solve_part_1(EASY_EXAMPLE), 18);
        assert_eq!(solve_part_2(EXAMPLE_INPUT), 54);
    }
}
