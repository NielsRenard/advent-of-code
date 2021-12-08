#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment)]

use std::{collections::{hash_map, HashMap, HashSet}, hash::Hash, ops::{Add, Mul, RangeBounds, Sub}};
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/8.input").unwrap()
}

    /*                      AAAA             dddd
       _   _       _       B    C           e    a
    |  _|  _| |_| |_       B    C           e    a
    | |_   _|   |  _|       DDDD    →→→      ffff
     _   _   _   _   _     E    F           g    b
    |_    | |_| |_| | |    E    F           g    b
    |_|   | |_|  _| |_|     GGGG             cccc

    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";

    1 is the only digit that uses 2 segments: ab
    7 is the only digit that uses 3 segments: dab
    4 is the only digit that uses 4 segments: eafb
    8 is the only digit that uses 7 segments: acedgfb
    2 3 and 5 use 5 segments:         cdfbe | gcdfa  | fbcad
    0 6 and 9 use 6 segments:        cefabd | cdfgeb | cagedb

    Below is just one example how to deduce all the segments

    1 and 7 have 1 differing segment: A,                       ∎ A == d ∎
    9 is 4 + A  + ? = eafb + d + ? only one choice: cefabd     ∎ G == c ∎
    3 is A + G + D + 1 = d+c+a+b+? only one choice:  fbcad     ∎ D == f ∎
    B is 4 - A - D - 1 = eafb - f - d - a - b = e              ∎ B == e ∎
    E is 8 - 9 = acedgfb - cefabd  = g                         ∎ E == g ∎
    5 is the five segment number without 3 or E :    cdfbe
    C is 9 - 5 = cefabd - cdfbe = a                            ∎ C == a ∎
    F is 1 - C = ab - a = b                                    ∎ F == b ∎
    (2 is the five segment number that isn't 3 or 5: gcdfa)

    */

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 2: {}", answer_2);
    // execution took 0.89ms
}

fn solve_part_one(input: &str) -> i32 {
    input
        .lines()
        .map(|l| l.split("| "))
        .map(|mut parts| parts.nth(1).unwrap())
        .map(|s| s.split_whitespace())
        .flat_map(|words| words.filter(|w| [2, 3, 4, 7].contains(&(w.len()))))
        .count()
        .try_into()
        .unwrap()
}

fn solve_part_two(input: &str) -> i32 {
    let lines: Vec<(Vec<&str>, Vec<&str>)> = parse_to_patterns_and_outputs(input);
    let mut output_values: Vec<i32> = vec![0];
    for mut line in lines {
        let output_value = calculate_output_value(line);
        output_values.push(output_value);
    }
    output_values.iter().sum()
}

fn calculate_output_value((mut patterns, output): (Vec<&str>, Vec<&str>)) -> i32 {
    patterns.sort_by(|a, b| a.len().partial_cmp(&b.len()).unwrap());
    let one = patterns[0].to_owned();
    let seven = patterns[1].to_owned();
    let four = patterns[2].to_owned();
    let eight: &str = patterns[9];
    let five_segment_digits: [&str; 3] = [patterns[3], patterns[4], patterns[5]];
    let six_segment_digits: [&str; 3] = [patterns[6], patterns[7], patterns[8]];

    let segment_a            = get_differences(&seven, &one)[0];
    let (nine, segment_g)    = get_segment_g(&four, &segment_a, six_segment_digits);
    let (three, segment_d)   = get_segment_d(&segment_a, &segment_g, &one, five_segment_digits);
    let zero                 = get_zero(eight, &segment_d, six_segment_digits);
    let six                  = six_segment_digits.iter().find(|pattern| !(**pattern == nine || **pattern == zero)).unwrap();
    let segment_b            = get_differences(&four, format!("{}{}", segment_d, &one).as_str())[0];
    let segment_e            = get_differences(eight, format!("{}{}{}{}{}", segment_a, segment_b, segment_d, segment_g, &one).as_str(),)[0];
    let five                 = get_five(&three, &segment_e, five_segment_digits);
    let segment_c            = get_differences(&nine, &five)[0];
    let segment_f            = one.chars().find(|c| *c != segment_c).unwrap();
    let two                  = five_segment_digits.iter().find(|pattern| !(**pattern == three || **pattern == five)).unwrap();

    let patterns: [&str; 10] = [&zero, &one, *two, &three, &four, &five, six, &seven, eight, &nine];

    let mut digits: Vec<usize> = vec![];
    for output_pattern in output  {
        let matched = patterns.iter().find(|pattern|
                                             output_pattern.len() == pattern.len()
                                             && output_pattern.chars().all(|c| pattern.contains(c))).unwrap();
        let digit = patterns.iter().position(|pattern| pattern == matched).unwrap();
        digits.push(digit)
    }

    let digits_as_string: String = digits.iter().map(ToString::to_string).collect::<String>();
    digits_as_string.parse().unwrap()
}

fn get_differences(larger: &str, smaller: &str) -> Vec<char> {
    let item_set: HashSet<char> = smaller.chars().into_iter().collect();
    larger.chars().into_iter().filter(|item| !item_set.contains(item)).collect()
}

fn get_segment_g(four: &str, segment_a: &char, six_segment_digits: [&str; 3]) -> (String, char) {
    let mut four_plus_a = String::from(four);
    four_plus_a.push(*segment_a);
    let nine: &&str = six_segment_digits
        .iter()
        .filter(|pattern| four_plus_a.chars().into_iter().all(|c| pattern.contains(c)))
        .collect::<Vec<&&str>>().get(0).unwrap();
    (nine.to_string(), get_differences(nine, &four_plus_a)[0])
}

fn get_segment_d(
    segment_a: &char,
    segment_g: &char,
    one: &str,
    five_segment_digits: [&str; 3],
) ->  (String, char) {
    let mut a_plus_g_plus_one = format!("{}{}{}", segment_a, segment_g, &one);
    let three: &&str = five_segment_digits
        .iter()
        .filter(|pattern| {
            a_plus_g_plus_one
                .chars()
                .into_iter()
                .all(|c| pattern.contains(c))
        })
        .collect::<Vec<&&str>>()
        .get(0)
        .unwrap();
    (three.to_string(), get_differences(three, &a_plus_g_plus_one)[0])
}

fn get_five(three: &str, segment_e: &char, five_segment_digits: [&str; 3],
) -> String {
    let five: &&str = five_segment_digits
        .iter()
        .filter(|pattern| pattern != &&three && !pattern.contains(*segment_e))
        .collect::<Vec<&&str>>()
        .get(0)
        .unwrap();
    five.to_string()
}

fn get_zero(eight: &str, segment_d: &char, six_segment_digits: [&str; 3],
) -> String {
    let zero: &&str = six_segment_digits
        .iter()
        .filter(|pattern| pattern != &&eight && !pattern.contains(*segment_d))
        .collect::<Vec<&&str>>()
        .get(0)
        .unwrap();
    zero.to_string()
}

fn parse_to_patterns_and_outputs(input: &str) -> Vec<(Vec<&str>, Vec<&str>)> {
    input
        .lines()
        .peekable()
        .map(|l| l.split(" | "))
        .map(|mut parts| {
            (
                parts.next().unwrap().split_ascii_whitespace().collect(),
                parts.next().unwrap().split_ascii_whitespace().collect(),
            )
        })
        .collect()
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str =
        "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";

const LARGER_EXAMPLE_INPUT : &str = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce";


    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(LARGER_EXAMPLE_INPUT), 26);
    }

    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 5353);
    }

    #[test]
    fn test_2_large() {
        assert_eq!(solve_part_two(LARGER_EXAMPLE_INPUT), 61229);
    }
}
