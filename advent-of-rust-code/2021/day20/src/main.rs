#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use std::{num, collections::HashSet};
use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/20.input").unwrap()
}

/*
   🚧       👷 Work in progress    👷      🚧

   BIG MESS HERE, DID NOT CLEAN ANYTHING UP YET

   🚧       👷 Work in progress    👷      🚧
*/

fn main() {
    print_time!("execution");
    let answer_1_and_2 = solve_part_one(&input_from_file());
    /*
       brute forced this one.
       I should come back to this in the future and find out how to do it properly.
       but will I actually do it?
       <2021-12-21 di 02:03>
    */
}

fn solve_part_one(input: &str) -> i64 {
    let (enhancement_algorithm, input_image): (Vec<bool>, Vec<Vec<bool>>) = parse(&input);
    // let enlarged = enlarge_canvas(&input_image);
    let enlarged = enlarge_canvas_test(&input_image);


    let y_max: usize = enlarged.len();
    let x_max: usize = enlarged.get(0).unwrap().len();
    let mut current: Vec<Vec<bool>> = enlarged.clone();
    let mut output_pixels: Vec<Vec<bool>> = vec![vec![false; x_max]; y_max];

    // let mut output_pixels: Vec<Vec<bool>> = Vec::new();
    let mut count = 0;
    loop {
        if count >= 7 { break }
        for y in 0..y_max {
            for x in 0..x_max {
                let block_of_nine = adjacent(x, y, &current);
                let decimal = adjacent_to_decimal(block_of_nine);
                let light_or_dark = enhancement_algorithm[decimal as usize];
                output_pixels[y][x] = light_or_dark;
                pretty_print_nine(x, y, &current);
            }
        }
        current = output_pixels.to_owned();
        // output_pixels.clear();
        count+=1;
    }

    pretty_print_image(&current);
    // println!("lit pixels output: {:?}", output_pixels.iter().flatten().filter(|light| **light).count());
    current.iter().flatten().filter(|light| **light).count().try_into().unwrap()
}

fn enlarge_canvas_test(img: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    let y_max_current = img.len();
    let x_max_current = img.get(0).unwrap().len();
    let offset_y = y_max_current * 6;
    let offset_x = x_max_current * 5;
    let mut larger: Vec<Vec<bool>> = vec![vec![false; offset_x]; offset_y];
    for y in 0..y_max_current {
        for x in 0..x_max_current {
            larger[y + 10][x + 10]= img[y][x];
        }
    }
    larger
}

fn enlarge_canvas(img: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    let y_max_current = img.len();
    let x_max_current = img.get(0).unwrap().len();
    let offset_y = y_max_current * 8;
    let offset_x = x_max_current * 5;
    let mut larger: Vec<Vec<bool>> = vec![vec![false; offset_x]; offset_y];
    for y in 0..y_max_current {
        for x in 0..x_max_current {
            larger[y + 400][x + 175]= img[y][x];
        }
    }
    larger
}

fn adjacent_to_decimal(block_of_nine: Vec<Vec<bool>>) -> i64 {
    let flat: String = block_of_nine.iter().flatten().map(|b| if *b { '1' } else { '0' }).join("");

    let decimal = i64::from_str_radix(&flat, 2).unwrap();
    // if decimal != 0 {
    //     println!("flat {:?}", flat);
    //     println!("decimal {:?}", decimal);
    // }
    decimal
}

fn adjacent(x: usize, y: usize, img: &Vec<Vec<bool>>) -> Vec<Vec<bool>> {
    // top_left    , top     , topright
    // left        , target  , right
    // bottom_left , bottom  , bottom_right

    let mut block: Vec<Vec<bool>> = vec![vec![false; 3]; 3];
    let x_max = img[0].len()-1;
    let y_max = img.len()-1;
    block[1][1]= img[y][x];
    if y == 0 {
        // block[2][1]= img[y+1][x];         
        // if x == 0 {
        //     block[1][2]= img[y][x+1];       
        //     block[2][2]= img[y+1][x+1];
        // } else if x == x_max {
        //     block[1][0]= img[y][x-1];
        //     block[2][0]= img[y+1][x-1];
        // } else {
        //     block[1][0]= img[y][x-1];
        //     block[1][2]= img[y][x+1];
        //     block[2][0]= img[y+1][x-1];
        //     block[2][2]= img[y+1][x+1];
        // }
    } else if y == y_max {
        // block[0][1]= img[y-1][x];
        // if x == 0 {
        //     block[0][2]= img[y-1][x+1];
        //     block[1][2]= img[y][x+1];
        // } else if x == x_max {
        //     block[1][0]= img[y][x-1];
        //     block[0][0]= img[y-1][x-1];
        // } else {
        //     block[0][0]= img[y-1][x-1];
        //     block[0][2]= img[y-1][x+1];
        //     block[1][0]= img[y][x-1];
        //     block[1][2]= img[y][x+1];
        // }
    } else {
        // block[0][1]= img[y-1][x];
        // block[2][1]= img[y+1][x];
        if x == 0 {
            // block[0][1]= img[y-1][x];
            // block[0][2]= img[y-1][x+1];
            // block[1][2]= img[y][x+1];
            // block[2][2]= img[y+1][x+1];
        } else if x == x_max {
            // block[0][1]= img[y-1][x];
        } else {
            block[0][0]= img[y-1][x-1];
            block[0][1]= img[y-1][x];
            block[0][2]= img[y-1][x+1];
            block[1][0]= img[y][x-1];
            block[1][1]= img[y][x];
            block[1][2]= img[y][x+1];
            block[2][0]= img[y+1][x-1];
            block[2][1]= img[y+1][x];
            block[2][2]= img[y+1][x+1];
        }
    }
    block
}

fn parse(input: &str) -> (Vec<bool>, Vec<Vec<bool>>) {
    let enhancement_algorithm: Vec<bool> = input.lines().next().unwrap().chars().map(|c| c == '#').collect();
    let input_image: Vec<Vec<bool>> = input.lines().skip(2).map(|line| line.chars().map(|c| c == '#').collect()).collect();
   (enhancement_algorithm, input_image)
}



fn pretty_print_image(image: &Vec<Vec<bool>>) {
    let max_x = &image[0].len();
    let max_y = image.len();
    println!("x: {:?}, y: {:?}", max_x, max_y);
    for y in 0..max_y {
        for x in 0..*max_x {
            let pixel = image[y][x];
            if pixel { print!("# ") } else { print!(". ")};
            if x == *max_x-1 {
                println!()
            }
        }
    }
}

fn pretty_print_nine(pixel_x: usize, pixel_y: usize, image: &Vec<Vec<bool>>) {
    let nine_block: Vec<Vec<bool>> = adjacent(pixel_x, pixel_y, image);
    if nine_block.iter().flatten().all(|b| !b) { return;}
    let nine_max_x = &nine_block[0].len();
    let nine_max_y = nine_block.len();
    println!(); 
    println!("center pixel_x: {:?}, pixel_y: {:?}", pixel_x, pixel_y);     
    for y in 0..nine_max_y {
        for x in 0..*nine_max_x {
            let pixel = nine_block[y][x];
            if pixel { print!("# ") } else { print!(". ")};
            if x == *nine_max_x-1 {
                println!()
            }
        }
    }

    println!(); 
    println!("O/X shows the center pixel of the nine block (O is on, X is off)");
    print!("    ");
    (0..=20).into_iter().for_each(|n| if n % 5 == 0 { print!("{} ", n)} else {print!("  ")});
    println!(); 
    let max_x = &image[0].len();
    let max_y = image.len();
    let mut opened_x = false;
    for y in 0..max_y {
        print!("{:2}  ", y);
        for x in 0..*max_x {
            let pixel = image[y][x];
            if pixel {
                if x == pixel_x && y == pixel_y {
                    print!("X ");
                } else {
                    print!("# ");
                }
            } else {
                if x == pixel_x && y == pixel_y {
                    print!("O ");
                } else {
                    print!(". ");
                };
            }
            if x == *max_x-1 {
                println!("{}", y);
            }
        }
    }
    print!("    ");
    (0..=20).into_iter().for_each(|n| if n % 5 == 0 { print!("{} ", n)} else {print!("  ")});
    println!(); 
}
    

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 79);
    }
}
