#![allow(dead_code, unused_mut, unused_variables)]

pub use std::collections::HashMap;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/7.input").unwrap()
}

fn main() {
    print_time!("execution");
    //     thread 'main' has overflowed its stack
    //
    // oops! first attempt didn't expect directories with the same name
    // required a subtantial rewrite using paths instead of directory names
    // and of course it wasn't necessary to pass the test input :)
    //
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());

    println!("part 1: {answer_1}");
    println!("part 2: {answer_2}");
}

#[derive(Debug)]
pub enum Node {
    Directory(String),
    File(String, u32),
}

type FileSystem = HashMap<String, Vec<Node>>;

fn solve_part_one(input: &str) -> u32 {
    let file_system: FileSystem = build_filesystem(input);
    let dir_sizes: HashMap<String, u32> = get_directory_sizes(file_system);
    dir_sizes.values().filter(|v| **v <= 100_000).sum()
}

fn solve_part_two(input: &str) -> u32 {
    let file_system: FileSystem = build_filesystem(input);
    let dir_sizes: HashMap<String, u32> = get_directory_sizes(file_system);
    let space_available = 70_000_000 - dir_sizes.get("/").unwrap();
    let space_needed = 30_000_000 - space_available;
    *dir_sizes
        .values()
        .filter(|v| **v >= space_needed)
        .min()
        .unwrap()
}

fn get_directory_sizes(file_system: FileSystem) -> HashMap<String, u32> {
    let mut directories = HashMap::new();
    file_system.iter().for_each(|(dir, nodes)| {
        let size = get_directory_size(&file_system, dir);
        directories.insert(dir.to_string(), size);
    });
    directories
}

/// Recursively computes the size of all files in a directory as well
/// as those in its sub directories
fn get_directory_size(file_system: &FileSystem, current_dir: &str) -> u32 {
    let dir = file_system.get(current_dir).unwrap();
    let total_size: u32 = dir
        .iter()
        .map(|n| match n {
            Node::Directory(dir) => get_directory_size(file_system, dir),
            Node::File(_, size) => *size,
        })
        .sum();
    total_size
}

/// Builds a HashMap of all directories and their contents  
/// where the key is the directory path
/// and the value is a list of nodes (Directory or File)
fn build_filesystem(input: &str) -> HashMap<String, Vec<Node>> {
    let mut file_system = HashMap::new();
    file_system.insert("/".to_string(), vec![]);

    let mut current_path = "".to_string();

    for line in input.lines() {
        if line.starts_with("$ ls") {
            continue;
        }
        if line.starts_with("$ cd") {
            let target_dir = line.splitn(3, ' ').last().unwrap();
            match target_dir {
                "/" => {
                    current_path = "/".to_string();
                }
                ".." => {
                    // TODO: replace this stuff with std::path::Path
                    let rev_path = current_path.split('/').rev().collect::<Vec<&str>>();
                    let new_path = rev_path
                        .into_iter()
                        .skip(1)
                        .rev()
                        .collect::<Vec<&str>>()
                        .join("/");

                    current_path = new_path;
                }
                dir => {
                    current_path = if current_path == "/" {format!("/{dir}")} else {format!("{current_path}/{dir}")};
                    file_system.insert(current_path.clone(), vec![]);
                }
            }
            continue;
        }

        let cur_dir: &mut Vec<Node> = file_system.get_mut(&current_path).unwrap();
        if line.starts_with("dir") {
            let dir = line.split(' ').last().unwrap().to_string();
            let new_path = if current_path == "/" {format!("/{dir}")} else {format!("{current_path}/{dir}")};
            cur_dir.push(Node::Directory(new_path));
        } else {
            let (size, filename) = line.split_once(' ').unwrap();
            cur_dir.push(Node::File(filename.to_owned(), size.parse().unwrap()));
        }
    }
    file_system
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"#;

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 95437);
    }
    #[test]
    fn test_two() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 24933642);
    }
}
