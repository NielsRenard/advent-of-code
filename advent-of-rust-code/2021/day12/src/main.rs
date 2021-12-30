#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment)]

use std::collections::HashMap;
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/12.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    //execution took 0.3ms
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    //execution took 18.43ms
}

#[derive(Debug)]
struct Cave {
    connected_caves: Vec<usize>,
    small: bool,
}

fn solve_part_one(input: &str) -> u32 {
    let (cave_indexes, caves) = parse_input(input);

    let total_caves: usize = caves.len();
    let start_idx: usize = cave_indexes["start"];
    let end_idx: usize = cave_indexes["end"];

    // initialize the list of visited nodes, starts as all false
    let mut visited: Vec<bool> = vec![false; total_caves];
    // let answer: u32 = number_of_paths_to_end(&caves, &mut visited, start_idx, end_idx);
    number_of_paths_to_end(&caves, &mut visited, start_idx, end_idx)
}

fn solve_part_two(input: &str) -> u32 {
    let (cave_indexes, caves) = parse_input(input);

    let total_caves: usize = caves.len();
    let start_idx: usize = cave_indexes["start"];
    let end_idx: usize = cave_indexes["end"];

    // initialize the list of visited nodes, starts as all false
    let mut visited: Vec<bool> = vec![false; total_caves];
    part_two_number_of_paths_to_end(&caves, &mut visited, false, start_idx, start_idx, end_idx)
}

/*
  Depth First Search
*/
fn number_of_paths_to_end(
    caves: &[Cave],
    visited: &mut Vec<bool>,
    index: usize,
    end_index: usize,
) -> u32 {
    let current_cave = &caves[index];

    if index == end_index {
        // reached the end
        1
    } else if visited[index] && current_cave.small {
        // dead end, i.e. small cave which we've already visited
        0
    } else {
        //mark current cave as visited, in order to calculate the paths of its connected caves
        visited[index] = true;
        /*
        calculate the amount of other paths to end,
        i.e. the paths starting 1 step closer to the end than this one.
        */
        let number_of_paths_from_connected_caves_to_end: Vec<u32> = current_cave
            .connected_caves
            .iter()
            .map(|&next_position| number_of_paths_to_end(caves, visited, next_position, end_index))
            .collect();
        // then unmark it as visited so other paths can still visit it
        visited[index] = false;
        // return the amount of paths we've calculated.
        number_of_paths_from_connected_caves_to_end.iter().sum()
    }
}

/*
  Depth First Search
  difference with part 1 is it now keeps track of if it has already "double visited" one small cave.
*/
fn part_two_number_of_paths_to_end(
    caves: &[Cave],
    visited: &mut Vec<bool>,
    used_double_visit: bool,
    index: usize,
    start_index: usize,
    end_index: usize,
) -> u32 {
    let current_cave: &Cave = &caves[index];

    if index == end_index {
        // reached the end
        1
    } else if visited[index] && current_cave.small && used_double_visit {
        // dead end, i.e. small cave which we've already visited and we don't have a "double visit" left to use
        0
    } else {
        if visited[index] && index == start_index {
            return 0;
        }

        // possibly mark our current path as having "used" our double-visit
        let mut this_is_the_double_visit = false;
        if visited[index] && current_cave.small && !used_double_visit {
            this_is_the_double_visit = true;
        }

        let new_used_double_visit = used_double_visit || this_is_the_double_visit;

        //mark current cave as visited, in order to calculate the paths of its connected caves
        visited[index] = true;

        /*
        calculate the amount of other paths to end,
        i.e. the paths starting 1 step closer to the end than this one.
         */
        let number_of_paths_from_connected_caves_to_end: Vec<u32> = current_cave
            .connected_caves
            .iter()
            .map(|&next_position| {
                part_two_number_of_paths_to_end(
                    caves,
                    visited,
                    new_used_double_visit,
                    next_position,
                    start_index,
                    end_index,
                )
            })
            .collect();

        // if this visit "used" our one double-visit, the rest of the paths can no longer visit it, so keep it marked as visited
        if !this_is_the_double_visit {
            // however, if we still have our double-visit left to use
            // we want the rest of the paths to be able to visit this cave still
            visited[index] = false;
        }

        // return the amount of paths we've calculated.
        number_of_paths_from_connected_caves_to_end.iter().sum()
    }
}

fn parse_input(input: &str) -> (HashMap<String, usize>, Vec<Cave>) {
    let mut cave_indexes: HashMap<String, usize> = HashMap::new();
    let mut caves: Vec<Cave> = Vec::new();

    for line in input.lines() {
        let cave_names: Vec<&str> = line.split('-').collect();
        for name in &cave_names {
            if !cave_indexes.contains_key(*name) {
                // insert the cave's name into a hashmap
                // with an index pointing to its location in the Vec<Cave>
                cave_indexes.insert(name.to_string(), caves.len());
                caves.push(Cave {
                    connected_caves: Vec::new(),
                    small: name.to_lowercase() == *name,
                })
            }
        }

        // add each cave to the others list of connected caves
        // 1. find indexes, in the indexing HashMap
        let cave_one_idx: usize = cave_indexes[cave_names[0]];
        let cave_two_idx: usize = cave_indexes[cave_names[1]];
        // 2. use indexes to add the caves's indexes to each others list of connected caves
        caves[cave_one_idx].connected_caves.push(cave_two_idx);
        caves[cave_two_idx].connected_caves.push(cave_one_idx);
    }
    (cave_indexes, caves)
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "start-A
start-b
A-c
A-b
b-d
A-end
b-end";

    const SLIGHTLY_LARGER_EXAMPLE_INPUT: &str = "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc";

    const EVEN_LARGER_EXAMPLE_INPUT: &str = "fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW";

    #[test]
    fn test_1_small() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 10);
    }
    #[test]
    fn test_1_medium() {
        assert_eq!(solve_part_one(SLIGHTLY_LARGER_EXAMPLE_INPUT), 19);
    }
    #[test]
    fn test_1_large() {
        assert_eq!(solve_part_one(EVEN_LARGER_EXAMPLE_INPUT), 226);
    }

    #[test]
    fn test_2_medium() {
        assert_eq!(solve_part_two(SLIGHTLY_LARGER_EXAMPLE_INPUT), 103);
    }

    //  #[test]
    // fn test_2_large() {
    //     assert_eq!(solve_part_two(EVEN_LARGER_EXAMPLE_INPUT), 3509);
    // }
}
