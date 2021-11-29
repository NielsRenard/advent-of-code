#![allow(dead_code, unused_mut, unused_variables)]
use std::collections::VecDeque;

fn main() {
    let answer = solve_part_one(&input_from_file());
    println!("{}", answer);
}

fn solve_part_one(input: &str) -> i64 {
    let lines = input.lines();
    let strings: Vec<String> = lines
        .map(|line| line.chars().filter(|c| !c.is_whitespace()).collect())
        .collect();
    strings.iter().map(|expression| step(expression)).sum()
}

fn step(expression: &String) -> i64 {
    let mut operands: VecDeque<i64> = VecDeque::new();
    let mut operators: VecDeque<char> = VecDeque::new();

    println!("");
    println!("solving: {:?}", expression);
    for c in expression.chars() {
        println!("operands {:?}, operators {:?}", operands, operators);
        match c {
            ')' => {
                println!(") end bracket encountered. Time to calc!");
                let mut operations_in_sub_expression = VecDeque::new();
                let mut sub_expression_operands = VecDeque::new();
                while operators.back().unwrap().clone() != '(' {
                    operations_in_sub_expression.push_front(operators.pop_back().unwrap());
                    sub_expression_operands.push_front(operands.pop_back().unwrap());
                }
                sub_expression_operands.push_front(operands.pop_back().unwrap());
                let _opening_bracket = operators.pop_back();

                while !operations_in_sub_expression.is_empty() {
                    println!("Sub expression");
                    println!("{:?}", operations_in_sub_expression);
                    println!("{:?}", sub_expression_operands);

                    let operation = operations_in_sub_expression.pop_front();
                    let n1 = sub_expression_operands.pop_front().unwrap();
                    let n2 = sub_expression_operands.pop_front().unwrap();
                    if operation.unwrap() == '+' {
                        sub_expression_operands.push_front(n1 + n2)
                    } else {
                        sub_expression_operands.push_front(n1 * n2)
                    }
                }
                operands.push_back(sub_expression_operands[0]);
            }
            '(' => operators.push_back(c),
            '+' => operators.push_back(c),
            '*' => operators.push_back(c),
            _ => operands.push_back(c.to_digit(10).unwrap() as i64),
        }
        println!("...next char...");
    }
    while !operators.is_empty() {
        println!("End of line reached, cleaning up stacks");
        println!("operands {:?}, operators {:?}", operands, operators);

        let operation = operators.pop_front();
        let n1 = operands.pop_front().unwrap();
        let n2 = operands.pop_front().unwrap();
        if operation.unwrap() == '+' {
            operands.push_front(n1 + n2)
        } else {
            operands.push_front(n1 * n2)
        }
    }
    println!("operands {:?}, operators {:?}", operands, operators);

    operands[0]
}
// maybe: 86707

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_0() {
        println!("solving: 2 * 3 + (4 * 5)");
        assert_eq!(solve_part_one("2 * 3 + (4 * 5)"), 26);
    }

    #[test]
    fn test_1() {
        println!("solving: 1 + (2 * 3) + (4 * (5 + 6))");
        assert_eq!(solve_part_one("1 + (2 * 3) + (4 * (5 + 6))"), 51);
    }

    #[test]
    fn test_2() {
        println!("solving: 5 + (8 * 3 + 9 + 3 * 4 * 3)");
        assert_eq!(solve_part_one("5 + (8 * 3 + 9 + 3 * 4 * 3)"), 437);
    }

    #[test]
    fn test_3() {
        println!("solving: 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))");
        assert_eq!(
            solve_part_one("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"),
            12240
        );
    }

    #[test]
    fn test_4() {
        println!("solving: ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2");
        assert_eq!(
            solve_part_one("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"),
            13632
        );
    }
}

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2020/18.input").unwrap()
}
