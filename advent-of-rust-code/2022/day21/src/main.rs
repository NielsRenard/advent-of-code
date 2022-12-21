#![feature(result_option_inspect, string_extend_from_within)]
#![allow(dead_code)]

use std::collections::VecDeque;

use fxhash::FxHashMap;

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/21.input").unwrap()
}

#[doc(hidden)]
fn main() {
    env_logger::init();
    print_time!("execution");
    let answer_1: usize = solve_part_1(&input_from_file());
    println!("part 1: {answer_1}");
    solve_part_2(&input_from_file());

}

#[derive(Debug)]
enum Op {
    Plus,
    Min,
    Mult,
    Div,
    Close,
}

#[derive(Debug)]
enum Job {
    Math(Op, String, String),
    Number(usize),
}

fn solve_part_1(input: &str) -> usize {
    let monkeys: FxHashMap<&str, Job> = parse_input(input);
    let expression = parse_to_arithmatic(&monkeys, "root");
    trace!("{expression}");
    meval::eval_str(expression).unwrap() as usize
}

fn solve_part_2(input: &str) {
    let monkeys: FxHashMap<&str, Job> = parse_input(input);
    let humn = match monkeys.get("humn").unwrap() {
        Job::Number(n) => n,
        _ => panic!(),
    };
    // humn = 5

    let root = monkeys.get("root").unwrap();
    let (root1, root2) = match root {
        Job::Math(_, m1, m2) => (m1, m2),
        _ => panic!(),
    };

    let left_hand_expr = parse_to_arithmatic(&monkeys, root1);
    let right_hand_expr = parse_to_arithmatic(&monkeys, root2);


    let (_unsolved, solvable) = if left_hand_expr.contains(&humn.to_string()) {
        //hmn is in the left hand expr
        (left_hand_expr, right_hand_expr)
    } else {
        (right_hand_expr, left_hand_expr)
    };

    let solved_value = meval::eval_str(solvable).unwrap();

    /*
    // replace the 5 in the left hand side with x, solve for x
    (((4+(2*(x-3)))/4) = ((32-2)*5))
    (((4+(2*(x-3)))/4) = 150
             (2*x-2)/4 = 150
                 2*x-2 = 600
                   2*x = 602
                     x = 301
 
    for real input the formula is below, have fun (or use wolfram alpha)

       (2*((((((((7*13)-9)+(6*3))+((4+7)+(((4*(3*3))+(12*2))-19)))+(4+(4+3)))*((((3*(9-2))*2)-11)*((3*((13+15)+(((5*((3*8)-1))-((10+19)+(4*2)))/2)))+((13+((2*(13-2))*9))+((12*((2+9)*3))-(((((2*5)+15)-(3+5))*5)+(4*10)))))))*((((2*((((2*((6*2)+11))/2)*(17+2))+(2*(((7*(1+6))+((((3*(3*(2+7)))+(((16+1)*5)+12))/2)*3))+(3*(1+6))))))*((((5*5)*((2+5)*5))+((((5*9)*7)+((2*9)+((2*(5*13))/2)))*4))+(((((5*(((2*7)/2)*2))*((((((4+(3*6))+(1+15))+(3*3))*2)-(3*9))+(3*((4+3)*(1+(3*2))))))/4)-((3*(6+1))*((3+4)+(2+(2*11)))))/2)))+(((((((2*(2+(5*((6+11)+8))))+((4*5)*(((6*9)+7)+6)))+(9*((7*3)+(3+(((11+((8*3)/2))*3)/3)))))+(((((3+(3*2))*(5+1))+13)+(9*16))+((2*((((((4+2)+3)*8)+(14*3))-(2*(4*4)))+((5*(5*2))+((3*((5*2)+19))-(5*4)))))+(((((3+(2*13))*2)+(10*(((13*3)/3)*4)))+(4*(5*5)))-(((9*3)*3)+2)))))*2)-((((((((((2*(18-2))*16)/(6+2))+(2+((2+(((2*(2*5))/2)+17))+8)))-14)+5)/2)*3)+(2*((((((5*7)-(2*4))+((((3+4)*2)*2)-7))-3)+(((2*3)*2)*(((5+1)+1)*2)))+(2*8))))*3))*((2*((3*((11*4)/2))+((5*(((2+((11*7)/7))*2)+1))-(2*(7+4)))))*(4*2))))+((((((4*3)*8)-(5*5))*4)+((((2+(4*2))*11)-((3*2)+3))*(19-6)))*((16*((2*((2*((2*((16+13)+2))+((5*(3*7))/3)))/2))+12))+(((((((2+17)*2)*((2+19)/3))+3)*2)+(((4*(8+(3*3)))/4)*5))*3)))))-((((12+(((5*2)*4)+(1+(2*3))))*(15-4))+((((3*(5*7))+(20*(4*4)))+(((((((((2*((((((2*(((((8*3)*(3*3))/(5+1))+((7+12)+6))*2))/2)*7)+((((((((2*(8+((2+(19+8))-8)))/2)*2)*8)+((((3*4)+((2*4)+(3*3)))*3)+(((((2*((11*((((((((((((3*(((5+2)*(2*(2+4)))+((2*(((((5+(3*16))*3)/3)*2)/2))+(3*17))))+((((((5*3)*2)*(((3*2)+11)+5))+((2*((((((2*((((((((2*(13+(3*8)))+(((2*5)+16)+3))*5)+(((5*(((10*5)+3)+(10+(((3*15)+(19*2))+((3*5)*3)))))+(2*(((3*(((1+(3*(5*2)))*2)/2))*(((((2*((12+5)+5))*5)+(((((((3*(((4*(7*2))-13)+(3*6)))+(((1+(2*6))*(((((5*2)*4)-9)+17)-5))+(((2*((2*((((14+5)+((3+(4*2))*2))*2)/2))*2))/(2*4))-5)))/2)*2)+19)+((((4+4)*((((3*3)+((17+14)*3))+(13*((3*3)+(13+9))))+x))-((((1+(2*3))+(16*6))*5)+(3*(3*(((3+3)+1)*3)))))/8))+((((14+(13*2))*(15+3))-(((14+3)*11)-(1+((((2*3)+1)+4)+((2*((4*2)-1))*3)))))+((3+(5*2))*(((3+4)*4)+3)))))/3)-(3*((3+4)*7))))-(2*(((6*3)*3)+(7*5))))))/5))/6)-(((((3*5)+(4*4))-2)*((6*13)/(8-2)))-((((2*(3+(2*4)))*2)+((((3*2)*((3+4)+4))/2)+2))-(12*2))))*20)+(4*((2*8)+((((((2*(8+((((5*5)+((2*5)+3))/2)-4)))+12)/2)*3)*2)-(5+12))))))-(((((7*(2*(2*6)))-((((2*11)*5)/5)/2))*2)*2)+3))+((2*10)*(7*3)))+((((((6+7)*4)*2)+20)+(18+(3*(3*(6+1)))))+(4*2)))/2)+((2*(2+(3*((((((3*(2+(1+(2+4))))+4)*2)+(2*(1+(2*(2+9)))))/3)/4))))+(11*(((2*(4+9))+3)*2)))))-(((((3*2)+(7*5))*(2*3))/6)+((((((((3*2)*2)*(2+5))-(7*4))+((12*3)+2))+(2*11))+((7+(9+1))*19))*2))))+(((17+16)/3)*(5*2)))/3))*2)-((7*(5+(2*(2*3))))+((8*4)*(2+5))))/3)+((((5+(5+2))+1)*2)+(2*((2+((11*2)+7))+12))))*2)-((3*((3*3)+(5*4)))*6))/3)-((11*14)+(((18/2)+(10*5))*3)))/3)+((3*(14-1))*2)))+(4*(((((((((6*3)/2)+((4*2)*3))+((4+9)*2))+(((3*3)*12)-((2*(2+4))+(2*(3*4)))))-((11*2)+(3*4)))+(15*2))*2)/2))))-((3*((5*2)+(((((((3*(6+1))-6)*2)+2)-9)*2)+(3*9))))+(((3*16)+(5*(((6*2)*3)+(20-3))))-(4*5))))/4)-(12*((3*((13*4)+(3*14)))/6)))*2)))/3)-(3*((2*(((((4+(18-5))*11)-(2*(11+((17*2)/2))))*2)/2))-((2*(3*2))+(3*7)))))/2))*2)+(2*((2*((((16/2)*2)+(5*3))+((2*(2*((2*(4*2))+1)))+4)))+((3+10)*(((5+1)+4)+1))))))-(((((7*2)*3)+(1+(2*5)))*2)*(3*3)))/2)-((15*2)*7))*2)+(5+(3+(4*5))))/3)-((5+3)*((13+18)*3)))/(3+8)))*((14+(1+(8*2)))+(3*4))))/2)))
       =
       89661494901968

    the answer for me was: 3093175982595
     */
}

/// constructs an arithmatic expression, e.g. example input becomes:
/// (((4+(2*(5-3)))/4)+((32-2)*5))
fn parse_to_arithmatic(monkeys: &FxHashMap<&str, Job>, starting_monkey: &str) -> String {
    let mut monkey_stack: VecDeque<&str> = VecDeque::new();
    let mut op_stack: VecDeque<&Op> = VecDeque::new();

    monkey_stack.push_back(starting_monkey);

    let mut expression = String::new();

    while let Some(name) = monkey_stack.pop_back() {
        let job = monkeys.get(name).unwrap();
        match job {
            Job::Math(op, m1, m2) => {
                expression.push('(');
                op_stack.push_back(&Op::Close);
                op_stack.push_back(op);
                monkey_stack.push_back(m2);
                monkey_stack.push_back(m1);
            }
            Job::Number(n) => {
                expression.push_str(&n.to_string());
                if let Some(op) = op_stack.pop_back() {
                    match op {
                        Op::Close => balance_expression(&mut expression, &mut op_stack),
                        _ => apply_operation(&mut expression, op),
                    };
                }
            }
        };
    }
    expression
}

/// keeps inserting closing parens until there are no more
/// then inserts one more operator
fn balance_expression(expression: &mut String, op_stack: &mut VecDeque<&Op>) {
    expression.push(')');
    while let Some(Op::Close) = op_stack.back() {
        op_stack.pop_back();
        expression.push(')');
    }
    if let Some(op) = op_stack.pop_back() {
        apply_operation(expression, op);
    }
}

fn apply_operation(expression: &mut String, op: &Op) {
    match op {
        Op::Plus => expression.push('+'),
        Op::Min => expression.push('-'),
        Op::Mult => expression.push('*'),
        Op::Div => expression.push('/'),
        _ => panic!(),
    }
}

fn parse_input(input: &str) -> FxHashMap<&str, Job> {
    input
        .lines()
        .map(|s| {
            let (monkey, job_str) = s.split_once(": ").unwrap();
            let job: Job = if job_str.chars().all(|c| c.is_numeric()) {
                Job::Number(job_str.parse::<usize>().unwrap())
            } else {
                let [m1, op_str, m2]: [&str; 3] = job_str
                    .splitn(3, ' ')
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                let op: Op = match op_str {
                    "+" => Op::Plus,
                    "-" => Op::Min,
                    "*" => Op::Mult,
                    "/" => Op::Div,
                    _ => panic!(),
                };
                Job::Math(op, m1.to_owned(), m2.to_owned())
            };
            (monkey, job)
        })
        .collect()
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
"#;

    #[test]
    pub fn test_one() {
        env_logger::init();
        assert_eq!(solve_part_1(EXAMPLE_INPUT), 152);
    }
}
