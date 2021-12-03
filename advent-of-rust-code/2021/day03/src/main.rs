#![allow(dead_code, unused_mut, unused_variables)]
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/3.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    let answer_2 = solve_part_two(&input_from_file());
    println!("part 1: {}", answer_1);
    println!("part 2: {}", answer_2);
    // execution took 0.75ms
}

fn single_digit_to_int(c: char) -> i32 {
    c.to_digit(10).unwrap().try_into().unwrap()
}

fn solve_part_one(input: &str) -> i64 {
    let number_of_digits: usize = input.lines().next().unwrap().len();
    let number_of_lines: usize = input.lines().count();

    let mut columns: Vec<Vec<i32>> = Vec::new();
    for n in 0..number_of_digits {
        let column = input
            .lines()
            .map(|line| single_digit_to_int(line.chars().nth(n).unwrap()))
            .collect();
        columns.push(column);
    }

    let mut most_common_bits: String = String::new();

    for n in 0..number_of_digits {
        let ones = columns
            .iter()
            .nth(n)
            .unwrap()
            .iter()
            .filter(|bit| **bit == 1)
            .count();
        let most_common_bit = if ones > (number_of_lines / 2) {
            '1'
        } else {
            '0'
        };
        most_common_bits.push(most_common_bit);
    }

    let least_common_bits: String = most_common_bits
        .replace("1", "temp_1")
        .replace("0", "1")
        .replace("temp_1", "0");

    let most_common_decimal = isize::from_str_radix(&most_common_bits, 2).unwrap();
    let least_common_decimal = isize::from_str_radix(&least_common_bits, 2).unwrap();
    most_common_decimal as i64 * least_common_decimal as i64
}

fn solve_part_two(input: &str) -> i64 {
    let number_of_digits: usize = input.lines().next().unwrap().len();
    let number_of_lines: usize = input.lines().count();

    let mut numbers: Vec<Vec<i32>> = input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| single_digit_to_int(c))
                .collect::<Vec<i32>>()
        })
        .collect();
    let mut numbers_oxygen = numbers.clone();
    let mut numbers_co2 = numbers.clone();

    for n in 0..number_of_digits {
        if numbers_oxygen.len() > 1 {
            let column: Vec<i32> = numbers_oxygen.iter().map(|number| number[n]).collect();
            let ones = column.iter().filter(|bit| **bit == 1).count();
            let zeroes = column.iter().filter(|bit| **bit == 0).count();
            let most_common_bit = if ones >= zeroes { 1 } else { 0 };
            numbers_oxygen.retain(|col| *col.get(n).unwrap() == most_common_bit);
        }
    }
    let oxygen_number = &numbers_oxygen[0];
    let oxygen_string: String = oxygen_number
        .iter()
        .map(|digit| digit.to_string())
        .collect::<Vec<String>>()
        .join("");
    let oxygen: i64 = isize::from_str_radix(&oxygen_string, 2)
        .unwrap()
        .try_into()
        .unwrap();

    let mut numbers_co2 = numbers.clone();
    for n in 0..number_of_digits {
        if numbers_co2.len() > 1 {
            let column: Vec<i32> = numbers_co2.iter().map(|number| number[n]).collect();
            let ones = column.iter().filter(|bit| **bit == 1).count();
            let zeroes = column.iter().filter(|bit| **bit == 0).count();
            let retain_bit = if zeroes > ones { 1 } else { 0 };
            numbers_co2.retain(|col| *col.get(n).unwrap() == retain_bit);
        }
    }
    let co2_number = &numbers_co2[0];
    let co2_string: String = co2_number
        .iter()
        .map(|digit| digit.to_string())
        .collect::<Vec<String>>()
        .join("");
    let co2: i64 = isize::from_str_radix(&co2_string, 2)
        .unwrap()
        .try_into()
        .unwrap();

    println!("oxygen: {:?}, co2: {:?}", oxygen, co2);
    oxygen * co2
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
";
    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 198);
    }
    #[test]
    fn test_2() {
        assert_eq!(solve_part_two(EXAMPLE_INPUT), 230);
    }
}
