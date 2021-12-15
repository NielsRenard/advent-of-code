#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]
use itertools::Itertools;
use std::{collections::HashMap, hash::Hash};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/15.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    //execution took 80ms
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    //execution took 56 sec (sorry...)
}

fn solve_part_one(input: &str) -> u32 {
    let grid = parse(input);
    // grid.iter().for_each(|l| println!("{:?}",  l));
    dijkstra(grid)
}

fn solve_part_two(input: &str) -> u32 {
    let grid: Vec<Vec<u32>> = parse(input);
    let grids: Vec<Vec<Vec<u32>>> = (0..5)
        .into_iter()
        .map(|n| {//\\//\\//\\//\\//\\//\\//\\/HADOUUUKEN
            grid.iter()//\\//\\//\\//\\//\\//\\//\\/HADOUUUKEN
                .map(|row| {//\\//\\//\\//\\//\\//\\//\\/HADOUUUKEN
                    row.iter()//\\//\\//\\//\\//\\//\\//\\/HADOUUUKEN
                        .map(|number| {//\\//\\//\\//\\//\\///\\/HADOUUUKEN
                            let new_number = number + n; //\\\\//\/\\/HADOUUUKEN
                            if new_number > 9 {//\\//\\//\\/\\//\\/HADOUUUKEN
                                new_number - 9//\\//\\//\\\\//\\/HADOUUUKEN
                            } else {//\\//\\//\\//\\//\/\\//\\/HADOUUUKEN
                                new_number//\\//\\////\\//\\/HADOUUUKEN
                            }//\\//\\//\\//\\//\\//\\//\\/HADOUUUKEN
                        })//\\//\\//\\//\\//\\//\\//\\/HADOUUUKEN
                        .collect_vec()//\\//\\////\\/HADOUUUKEN
                })//\\//\\//\\//\\//\\//\\//\\/HADOUUUKEN
                .collect()//\\//\\//\\/\//\\/HADOUUUKEN
        })
        .collect_vec();

    let long_grid: Vec<Vec<u32>> = grids.into_iter().flatten().collect_vec();
    let wide_grid = widen_grid(long_grid);
    dijkstra(wide_grid)
}

fn widen_grid(grid: Vec<Vec<u32>>) -> Vec<Vec<u32>> {
    let mut long_lines: Vec<Vec<u32>> = vec![];
    for line in grid {
        let long_line = (0..5)
            .into_iter()
            .flat_map(|n| {
                line.iter().map(move |risk| {
                    let new_number = risk + n;
                    if new_number > 9 {
                        new_number - 9
                    } else {
                        new_number
                    }
                })
            })
            .collect_vec();
        long_lines.push(long_line);
    }
    long_lines
}

fn dijkstra(grid: Vec<Vec<u32>>) -> u32 {
    let x_max = grid[0].len();
    let y_max = grid.len();
    let mut visited: Vec<Vec<bool>> = vec![vec![false; x_max]; y_max];
    visited[0][0] = true;
    let mut cost_grid: Vec<Vec<u32>> = vec![vec![9999; x_max]; y_max];
    cost_grid[0][0] = 0;

    let (mut node_x, mut node_y): (usize, usize) = (0, 0);
    let mut keep_going = 1;
    while visited.iter().flatten().any(|visited| !visited) {
        let cost = cost_grid[node_y][node_x];
        if (node_x, node_y) == (x_max, y_max) {
            break;
        }
        if node_y > 0 {
            let top @ (top_x, top_y) = (node_x, node_y - 1);
            let top_val = grid[top_y][top_x];
            let top_entry = ((top_x, top_y), top_val);
            let new_cost_top = cost + top_val;
            if new_cost_top < cost_grid[top_y][top_x] {
                cost_grid[top_y][top_x] = new_cost_top;
            }
        }
        if node_x > 0 {
            let left @ (left_x, left_y) = (node_x - 1, node_y);
            let left_val = grid[left_y][left_x];
            let left_entry = ((left_x, left_y), left_val);
            let new_cost_left = cost + left_val;
            if new_cost_left < cost_grid[left_y][left_x] {
                cost_grid[left_y][left_x] = new_cost_left;
            }
        }

        if node_y < y_max - 1 {
            let bot @ (bot_x, bot_y) = (node_x, node_y + 1);
            let bot_val = grid[bot_y][bot_x];
            let new_cost_bot = cost + bot_val;
            if new_cost_bot < cost_grid[bot_y][bot_x] {
                cost_grid[bot_y][bot_x] = new_cost_bot;
            }
        }

        if node_x < x_max - 1 {
            let right @ (right_x, right_y) = (node_x + 1, node_y);
            let right_val = grid[right_y][right_x];
            let new_cost_right = cost + right_val;
            if new_cost_right < cost_grid[right_y][right_x] {
                cost_grid[right_y][right_x] = new_cost_right;
            }
        }

        let mut cheapest_neighbor: (usize, usize, u32) = (0, 0, 9999);
        for (y, row) in cost_grid.iter().enumerate() {
            for (x, cost) in row.iter().enumerate() {
                if cost < &cheapest_neighbor.2 && !visited[y][x] {
                    cheapest_neighbor = (x, y, *cost);
                }
            }
        }
        (node_x, node_y) = (cheapest_neighbor.0, cheapest_neighbor.1);
        visited[node_y][node_x] = true;
        keep_going += 1;
    }

    // for (key, value) in &distance_map {
    //     println!("DISTANCE MAP {:?}: {:?}", key, value);
    // }
    // for line in &cost_grid {
    //     println!("COST GRID   {:?}", line);
    // }
    // for line in &parent_grid {
    //         println!("PARENT GRID {:?}", line);
    // }
    // print(&visited);
    cost_grid[y_max - 1][x_max - 1]
}

fn parse(input: &str) -> Vec<Vec<u32>> {
    let grid = input
        .lines()
        .map(|row| {
            row.chars()
                .into_iter()
                .map(|c| char::to_digit(c, 10).unwrap())
                .collect_vec()
        })
        .collect_vec();
    grid
}

fn print(matrix: &[Vec<bool>]) {
    let width = matrix[0].len();
    matrix.iter().for_each(|row| {
        row.iter().enumerate().for_each(|(idx, n)| {
            if idx + 1 < width {
                if !*n {
                    print!(" .")
                } else {
                    print!(" #")
                }
            } else if !*n {
                println!(" .")
            } else {
                println!(" #")
            }
        })
    })
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581";

    const PART_TWO_EXAMPLE_INPUT: &str = "11637517422274862853338597396444961841755517295286
13813736722492484783351359589446246169155735727126
21365113283247622439435873354154698446526571955763
36949315694715142671582625378269373648937148475914
74634171118574528222968563933317967414442817852555
13191281372421239248353234135946434524615754563572
13599124212461123532357223464346833457545794456865
31254216394236532741534764385264587549637569865174
12931385212314249632342535174345364628545647573965
23119445813422155692453326671356443778246755488935
22748628533385973964449618417555172952866628316397
24924847833513595894462461691557357271266846838237
32476224394358733541546984465265719557637682166874
47151426715826253782693736489371484759148259586125
85745282229685639333179674144428178525553928963666
24212392483532341359464345246157545635726865674683
24611235323572234643468334575457944568656815567976
42365327415347643852645875496375698651748671976285
23142496323425351743453646285456475739656758684176
34221556924533266713564437782467554889357866599146
33859739644496184175551729528666283163977739427418
35135958944624616915573572712668468382377957949348
43587335415469844652657195576376821668748793277985
58262537826937364893714847591482595861259361697236
96856393331796741444281785255539289636664139174777
35323413594643452461575456357268656746837976785794
35722346434683345754579445686568155679767926678187
53476438526458754963756986517486719762859782187396
34253517434536462854564757396567586841767869795287
45332667135644377824675548893578665991468977611257
44961841755517295286662831639777394274188841538529
46246169155735727126684683823779579493488168151459
54698446526571955763768216687487932779859814388196
69373648937148475914825958612593616972361472718347
17967414442817852555392896366641391747775241285888
46434524615754563572686567468379767857948187896815
46833457545794456865681556797679266781878137789298
64587549637569865174867197628597821873961893298417
45364628545647573965675868417678697952878971816398
56443778246755488935786659914689776112579188722368
55172952866628316397773942741888415385299952649631
57357271266846838237795794934881681514599279262561
65719557637682166874879327798598143881961925499217
71484759148259586125936169723614727183472583829458
28178525553928963666413917477752412858886352396999
57545635726865674683797678579481878968159298917926
57944568656815567976792667818781377892989248891319
75698651748671976285978218739618932984172914319528
56475739656758684176786979528789718163989182927419
67554889357866599146897761125791887223681299833479";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 40);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 315);
    }
}
