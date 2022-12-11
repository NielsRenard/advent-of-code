#![allow(dead_code, unused_mut, unused_variables)]

pub use std::collections::HashMap;
use std::collections::VecDeque;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/11.input").unwrap()
}

//TODO: part 2
//TODO: write an actual monkey parser

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {answer_1}");
}

fn solve_part_one(_input: &str) -> usize {
    let monkey0 = Monkey {
        items: vec![76, 88, 96, 97, 58, 61, 67],
        operation: |old| old * 19,
        test: |x| if x % 3 == 0 { 2 } else { 3 },
    };

    let monkey1 = Monkey {
        items: vec![93, 71, 79, 83, 69, 70, 94, 98],
        operation: |old| old + 8,
        test: |x| if x % 11 == 0 { 5 } else { 6 },
    };

    let monkey2 = Monkey {
        items: vec![50, 74, 67, 92, 61, 76],
        operation: |old| old * 13,
        test: |x| if x % 19 == 0 { 3 } else { 1 },
    };

    let monkey3 = Monkey {
        items: vec![76, 92],
        operation: |old| old + 6,
        test: |x| if x % 5 == 0 { 1 } else { 6 },
    };

    let monkey4 = Monkey {
        items: vec![74, 94, 55, 87, 62],
        operation: |old| old + 5,
        test: |x| if x % 2 == 0 { 2 } else { 0 },
    };

    let monkey5 = Monkey {
        items: vec![59, 62, 53, 62],
        operation: |old| old * old,
        test: |x| if x % 7 == 0 { 4 } else { 7 },
    };

    let monkey6 = Monkey {
        items: vec![62],
        operation: |old| old + 2,
        test: |x| if x % 17 == 0 { 5 } else { 7 },
    };

    let monkey7 = Monkey {
        items: vec![85, 54, 53],
        operation: |old| old + 3,
        test: |x| if x % 13 == 0 { 4 } else { 0 },
    };

    let monkeys = vec![
        monkey0, monkey1, monkey2, monkey3, monkey4, monkey5, monkey6, monkey7,
    ];
    let count = count_inspect_items(monkeys);
    monkey_business(&count)
}

fn monkey_business(inspection_count: &[usize]) -> usize {
    let mut sorted = inspection_count.to_owned();
    sorted.sort();
    sorted.reverse();
    sorted[0..2].iter().product()
}

/// Simulates how many items each monkey inspects during x rounds
fn count_inspect_items(monkeys: Vec<Monkey>) -> Vec<usize> {
    let mut whos_turn = 0;
    let monkey_count = monkeys.len();
    let mut inspection_count = vec![0; monkeys.len()];

    let mut items: Vec<Vec<u32>> = monkeys
        .iter()
        .map(|monkey| monkey.items.clone())
        .collect::<Vec<Vec<u32>>>();
    let mut items_to_move: VecDeque<(usize, u32)> = VecDeque::new();

    (1..=20).for_each(|n| {
        println!("--------round {n}");
        loop {
            while let Some((dest, item)) = items_to_move.pop_front() {
                println!("moving {item} to monkey {dest}");
                items[dest].push(item);
            }
            println!("____monkey {whos_turn}");

            let monkey_items = &mut items[whos_turn];

            while !monkey_items.is_empty() {
                let item = monkey_items.drain(0..1).next().unwrap();
                inspection_count[whos_turn] += 1;
                let inspected_item = (monkeys[whos_turn].operation)(&item);
                let updated_item = inspected_item / 3;
                let pass_to_monkey = (monkeys[whos_turn].test)(&updated_item);
                items_to_move.push_back((pass_to_monkey, updated_item));
            }

            if whos_turn == monkey_count - 1 {
                whos_turn = 0;
                break;
            } else {
                whos_turn += 1;
            }
        }
        println!("{items:?}");
    });
    println!("inspection_count {inspection_count:?}");
    inspection_count
}

struct Monkey {
    items: Vec<u32>,
    operation: fn(&u32) -> u32,
    test: fn(&u32) -> usize,
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
"#;

    #[test]
    fn test_manual() {
        let monkey0 = Monkey {
            items: vec![79, 98],
            operation: |old| old * 19,
            test: |x| if x % 23 == 0 { 2 } else { 3 },
        };

        let monkey1 = Monkey {
            items: vec![54, 65, 75, 74],
            operation: |old| old + 6,
            test: |x| if x % 19 == 0 { 2 } else { 0 },
        };

        let monkey2 = Monkey {
            items: vec![79, 60, 97],
            operation: |old| old * old,
            test: |x| if x % 13 == 0 { 1 } else { 3 },
        };

        let monkey3 = Monkey {
            items: vec![74],
            operation: |old| old + 3,
            test: |x| if x % 17 == 0 { 0 } else { 1 },
        };

        let monkeys = vec![monkey0, monkey1, monkey2, monkey3];
        let count = count_inspect_items(monkeys);
        let business = monkey_business(&count);
        assert_eq!(business, 10605);
    }

    #[test]
    fn test_one() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 10605);
    }
}
