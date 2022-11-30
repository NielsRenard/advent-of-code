#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
    ops::{Range, RangeBounds, RangeInclusive},
};

use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/24.input").unwrap()
}

fn main() {
    print_time!("execution");
    let input = &input_from_file();
    let answer_1 = solve_part_one(input);
    println!("part 1: {:?}", answer_1);
    // execution took 370.24ms  <--- brute force

    // let answer_2 = solve_part_two(input);
    // println!("part 1: {:?}", answer_2);
    // // execution took 48.11ms
}

fn solve_part_one(input: &str) -> i64 {
    let instructions = parse(input);
    // println!("{:?}", instructions);
    // let mut model_number: i64 = 99999999999999;
    let mut model_number: i64 = 11111111111111;

    loop {
        let registers = [('w', 0),('x', 0), ('y', 0),('z', 0)];
        let mut registers : HashMap<char, i64> = std::collections::HashMap::from_iter(registers.into_iter());
        run_program(&mut registers, &instructions, model_number);

        if registers.get(&'z') == Some(&0) {
            // println!("REGISTERS:    {:?}", registers); 
            println!("valid model number: {:?}", model_number); 
            break;
        }

        // if model_number == 11111111111111 {
        //     println!("no 14 digit number is correct"); 
        //     break;
        // }

        model_number = decrease_model_number(model_number);
        // println!("MODEL_NUMBER: {:?}", model_number);
        // println!("{:?}", registers.get(&'z')); 
    }
    model_number
}

fn run_program(registers: &mut HashMap<char, i64>, instructions: &Vec<Instruction>, model_number: i64 ) {
    let mut digits = model_number.to_string().chars().map(|c| char::to_digit(c, 10).unwrap()).collect::<VecDeque<u32>>();
    for instruction in instructions {
        if digits.is_empty() { println!("☬"); break; }
        
        match instruction {
            Instruction::Inp(register) => {
                registers.insert(*register, digits.pop_front().unwrap() as i64);
                println!("DIGITS:    {:?}", digits); 

            }
            Instruction::AddDirect(register, value) => {
                *registers.get_mut(register).unwrap() += value;
            }
            Instruction::MulDirect(register, value) => {
                *registers.get_mut(register).unwrap() *= value;
            }
            Instruction::DivDirect(register, value) => {
                if value == &0 {
                    panic!("div by 0!");
                }
                *registers.get_mut(register).unwrap() /= value;
                // this probably will bug because of rounding errors
            }
            Instruction::ModDirect(register, value) => {
                let mut deref_value = *registers.get_mut(register).unwrap();
                if value <= &0 || deref_value < 0 {
                    panic!("modding by 0 or negative value, a= {:?}, b = {:?}", deref_value, value);
                }
                *registers.get_mut(register).unwrap() %= value;
            }
            Instruction::EqlDirect(register, value) => {
                let mut deref_val = *registers.get_mut(register).unwrap();
                *registers.get_mut(register).unwrap() = if deref_val == *value { 1 } else {  0 };
            }
            Instruction::AddRegister(register, other_register) => {
                *registers.get_mut(register).unwrap() += *registers.get_mut(other_register).unwrap();
            }
            Instruction::MulRegister(register, other_register) => {
                *registers.get_mut(register).unwrap() *= *registers.get_mut(other_register).unwrap();
            }
            Instruction::DivRegister(register, other_register) => {
                let mut other_deref_value = *registers.get_mut(other_register).unwrap();
                if other_deref_value == 0 {
                    panic!("div by 0!");
                }
                *registers.get_mut(register).unwrap() /= *registers.get_mut(other_register).unwrap();
                // likely going to bug because of rounding, see above
            }
            Instruction::EqlRegister(register, other_register) => {
                let mut deref_val = *registers.get_mut(register).unwrap();
                let mut deref_other_val = *registers.get_mut(other_register).unwrap();
                *registers.get_mut(register).unwrap() = if deref_val == deref_other_val { 1 } else {  0  };
            }
            _ => panic!()
        }
        let w = registers.get(&'w').unwrap();
        let x = registers.get(&'x').unwrap();
        let y = registers.get(&'y').unwrap();
        let z = registers.get(&'z').unwrap();
        println!("w: {:8?}  x: {:8?} y: {:8?} z: {:8?}", w,x,y,z);
    }

}

fn run_instruction((mut w, mut x, mut y, mut z): &(i32, i32,i32, i32), instruction: &Instruction) {
    match instruction {
        Instruction::Inp(register) =>  {},
        _ => panic!()
    }
    (w,x,y,z);
}

fn parse(input: &str) -> Vec<Instruction> {
    input.lines().map(|line| line.split_whitespace().collect_vec()).map(|parts| match parts[0]{
        "inp" => Instruction::Inp(parts[1].chars().next().unwrap()),
        "mod" => Instruction::ModDirect(parts[1].chars().next().unwrap(), parts[2].parse().unwrap()),
        _ => {
            let (operand_a, mut operand_b) = (parts[1].chars().next().unwrap(), parts[2]);
            let is_number = operand_b.chars().any(|c| char::is_digit(c, 10));            
            match parts[0] {
                "add" => {
                    if is_number {
                        Instruction::AddDirect(operand_a, operand_b.parse().unwrap())
                    } else {
                        Instruction::AddRegister(operand_a, operand_b.chars().next().unwrap())
                    }
                },
                "mul" => {
                    if is_number {
                        Instruction::MulDirect(operand_a, operand_b.parse().unwrap())
                    } else {
                        Instruction::MulRegister(operand_a, operand_b.chars().next().unwrap())
                    }
                },
                "div" => {
                    if is_number {
                        Instruction::DivDirect(operand_a, operand_b.parse().unwrap())
                    } else {
                        Instruction::DivRegister(operand_a, operand_b.chars().next().unwrap())
                    }
                },
                "eql" => {
                    if is_number {
                        Instruction::EqlDirect(operand_a, operand_b.parse().unwrap())
                    } else {
                        Instruction::EqlRegister(operand_a, operand_b.chars().next().unwrap())
                    }
                },
                _ => panic!()
            }
        }
        
    }).collect()
}

fn decrease_model_number(model_number: i64) -> i64{
    let mut decreased = model_number - 1;
    loop {
        if decreased.to_string().chars().any(|digit| digit == '0') {
            decreased -= 1;
        } else {
            break;
        }
    }
    decreased
}

#[derive(Debug)]
enum Instruction  {
    Inp(char),
    AddDirect(char, i64),
    MulDirect(char, i64),
    DivDirect(char, i64),
    ModDirect(char, i64),
    EqlDirect(char, i64),
    AddRegister(char, char),
    MulRegister(char, char),
    DivRegister(char, char),
    EqlRegister(char, char),
}



#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "inp x
mul x -1";

    const LARGER_EXAMPLE_INPUT: &str = "inp z
inp x
mul z 3
eql z x";

        const LARGEST_EXAMPLE_INPUT: &str = "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2";


    
    // #[test]
    // fn test_1() {
    //     assert_eq!(solve_part_one(EXAMPLE_INPUT), 39);
    // }

    // #[test]
    // fn test_1_larger() {
    //     assert_eq!(solve_part_one(LARGER_EXAMPLE_INPUT), 590784);
    // }

    #[test]
    fn test_division() {
        println!("9/2: {:?}", 9/2);
        println!("11/2: {:?}", 11/2);
        println!("1%26: {:?}", 1%26);
    }
    
    // #[test]
    // fn test_1_even_larger() {
    //     assert_eq!(solve_part_one(LARGEST_EXAMPLE_INPUT), 590784);
    // }
}
