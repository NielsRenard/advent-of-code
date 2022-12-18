#![feature(result_option_inspect, string_extend_from_within)]
#![allow(dead_code)]

use std::collections::VecDeque;

use fxhash::FxHashMap;

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/18.input").unwrap()
}

#[doc(hidden)]
fn main() {
    env_logger::init();
    print_time!("execution");
    let answer_1 = solve_part_1(&input_from_file());
    println!("part 1: {answer_1}");
    let answer_2 = solve_part_2(&input_from_file());
    println!("part 2: {answer_2}");
}

type Cube = (usize, usize, usize);

fn solve_part_1(input: &str) -> usize {
    let cubes: Vec<Cube> = parse_input(input);

    let mut cache: FxHashMap<Cube, Vec<Cube>> = FxHashMap::default();
    let mut surface_area = 0;

    for coord in &cubes {
        let cubes_around = cubes_around(&cubes, coord, &mut cache);
        surface_area += 6 - cubes_around.len();
    }
    surface_area
}

/// flood fill on 3d array
fn solve_part_2(input: &str) -> isize {
    let coordinates: Vec<Cube> = parse_input(input);
    let grid = map_coordinates_to_grid(&coordinates);

    let mut visited = [[[false; 30]; 30]; 30];
    let mut surface_area = 0;
    let mut queue = VecDeque::from([(1, 1, 1)]);

    visited[1][1][1] = true;
    while let Some(point) = queue.pop_front() {
        for adj_point in get_neighbor_indices(&point) {
            if within_grid(&adj_point) {
                let adj_x = adj_point.0 as usize;
                let adj_y = adj_point.1 as usize;
                let adj_z = adj_point.2 as usize;

                if grid[adj_x][adj_y][adj_z] {
                    surface_area += 1;
                    visited[adj_x][adj_y][adj_z] = true;
                    continue;
                }

                if !visited[adj_x][adj_y][adj_z] {
                    visited[adj_x][adj_y][adj_z] = true;
                    queue.push_back((adj_x, adj_y, adj_z));
                }
            }
        }
    }
    surface_area
}

fn within_grid(&(x, y, z): &(isize, isize, isize)) -> bool {
    0 < x && 0 < y && 0 < z && x < 30 && y < 30 && z < 30
}

fn get_neighbor_indices(&(x, y, z): &(usize, usize, usize)) -> [(isize, isize, isize); 6] {
    [
        (x as isize + 1, y as isize, z as isize),
        (x as isize - 1, y as isize, z as isize),
        (x as isize, y as isize + 1, z as isize),
        (x as isize, y as isize - 1, z as isize),
        (x as isize, y as isize, z as isize + 1),
        (x as isize, y as isize, z as isize - 1),
    ]
}

fn map_coordinates_to_grid(coordinates: &[(usize, usize, usize)]) -> [[[bool; 30]; 30]; 30] {
    let mut grid = [[[false; 30]; 30]; 30];

    for &(x, y, z) in coordinates {
        // shift everything up so don't have to deal with zeroes
        grid[x + 2][y + 2][z + 2] = true;
    }
    grid
}

fn cubes_around(
    cubes: &[Cube],
    coord @ (x, y, z): &Cube,
    cache: &mut FxHashMap<Cube, Vec<Cube>>,
) -> Vec<Cube> {
    if let Some(cubes) = cache.get(coord) {
        cubes.to_owned()
    } else {
        let cubes_around: Vec<(usize, usize, usize)> = [
            cubes.iter().find(|q| **q == (x - 1, *y, *z)),
            cubes.iter().find(|q| **q == (x + 1, *y, *z)),
            cubes.iter().find(|q| **q == (*x, y + 1, *z)),
            cubes.iter().find(|q| **q == (*x, y - 1, *z)),
            cubes.iter().find(|q| **q == (*x, *y, z + 1)),
            cubes.iter().find(|q| **q == (*x, *y, z - 1)),
        ]
        .into_iter()
        .flatten()
        .copied()
        .collect();

        cache.insert(*coord, cubes_around.clone());
        cubes_around
    }
}

fn parse_input(input: &str) -> Vec<(usize, usize, usize)> {
    input
        .lines()
        .map(|l| {
            let cube: Vec<usize> = l
                .splitn(3, ',')
                .map(|s| s.parse::<usize>().unwrap())
                .collect();
            (cube[0], cube[1], cube[2])
        })
        .collect()
}

fn print_3d_array_example_input(coordinates: &[Cube]) {
    let min_x = coordinates.iter().map(|(x, _, _)| x).min().unwrap();
    let max_x = coordinates.iter().map(|(x, _, _)| x).max().unwrap();
    let min_y = coordinates.iter().map(|(_, y, _)| y).min().unwrap();
    let max_y = coordinates.iter().map(|(_, y, _)| y).max().unwrap();
    let min_z = coordinates.iter().map(|(_, _, z)| z).min().unwrap();
    let max_z = coordinates.iter().map(|(_, _, z)| z).max().unwrap();

    let mut array = vec![vec![vec![0; 6]; 6]; 6];
    (*min_x..*max_x).for_each(|x| {
        for y in *min_y..*max_y {
            for z in *min_z..*max_z {
                array[x][y][z] = 1;
            }
        }
    });

    for i in 0..7 {
        for j in 1..4 {
            for k in 0..7 {
                if coordinates.contains(&(i, j, k)) {
                    print!("{:^5}", "⦵");
                } else {
                    print!("{:^5}", "▦");
                }
            }
            print!("       ");
        }
        println!();
    }
    println!();
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
"#;

    #[test]
    pub fn print_example() {
        let cubes: Vec<Cube> = parse_input(EXAMPLE_INPUT);
        print_3d_array_example_input(&cubes);
    }

    #[test]
    pub fn test() {
        // env_logger::init();
        assert_eq!(solve_part_1(EXAMPLE_INPUT), 64);
    }

    #[test]
    pub fn test_two() {
        env_logger::init();
        assert_eq!(solve_part_2(EXAMPLE_INPUT), 58);
    }
}
