#![allow(dead_code, unused_mut, unused_variables)]

use std::ops::Rem;
#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/25.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve(&input_from_file());
    println!("part 1: {answer_1}");
}

fn snafu_index() -> Vec<i64> {
    let mut snafu_index: Vec<i64> = vec![1];
    for _ in 0..19 {
        let previous = snafu_index.last().unwrap();
        snafu_index.push(previous * 5);
    }
    snafu_index
}

fn solve(input: &str) -> String {
    let snafu_index = snafu_index();
    println!("snafu index: {snafu_index:?}");

    let mut snafu_numbers: Vec<i64> = input.lines().map(|line| from_snafu(line)).collect();
    let decimal: i64 = snafu_numbers.iter().sum();
    let answer: String = to_snafu(decimal);
    answer
}

fn from_snafu(s: &str) -> i64 {
    let snafu_index = snafu_index();
    let mut num = 0;
    let rev_snaf = s.chars().rev().collect::<String>();
    rev_snaf.char_indices().for_each(|(idx, c)| match c {
        '-' => num += -snafu_index[idx],
        '=' => num += 2 * -(snafu_index[idx]),
        c => num += (char::to_digit(c, 10).unwrap() as i64) * snafu_index[idx],
    });
    num
}

fn to_snafu(mut number: i64) -> String {
    let mut snafu_digits = Vec::new();
    while number > 0 {
        println!("{number} is greater than 0");
        let remainder = number % 5;
        println!("remainder of {number} % 5 = {remainder}");
        snafu_digits.push(match remainder {
            0 => '0',
            1 => '1',
            2 => '2',
            3 => '=',
            4 => '-',
            _ => panic!(),
        });
        println!("snafu digits: {snafu_digits:?}");
        if remainder == 3 || remainder == 4 {
            // carry over
            number += 5 - remainder
        }
        println!("number /= 5: {}", number / 5);
        number /= 5;
    }
    snafu_digits.iter().rev().collect::<String>()
}

// fn to_sniffu(decimal: i64) -> String {
//     let snafu_index = snafu_index();
//     let (digits, biggest_fit) = snafu_index
//         .iter()
//         .enumerate()
//         .rev()
//         .find(|(i, &snafu)| snafu <= decimal)
//         .map(|(i, &snafu)| (i + 1, snafu))
//         .unwrap();

//     println!("biggest fit {biggest_fit}, digits: {digits}");
//     let mut snafu: String = (0..digits).map(|n| "2").collect();

//     while from_snafu(&snafu) != decimal {
//         snafu = reduce_snafu(&snafu);
//         println!("reduced : {snafu}, decimal: {}", from_snafu(&snafu));
//     }
//     println!("found answer: {snafu}");
//     snafu
// }

// fn bigger_snafu_than_decimal(decimal: i64) -> String {
//     let mut big_snafu = "2".to_string();
//     while from_snafu(&big_snafu) < decimal {
//         big_snafu.push('2');
//     }
//     big_snafu
// }

// /// 222 -> 221   == 61
// /// 221 -> 220   == 60
// /// 220 -> 22-   == 59
// /// 22- -> 22=   == 58
// /// 22= -> 212   == 57
// fn reduce_snafu(snafu: &str) -> String {
//     let mut reduce_char_idx = 1;

//     loop {
//         if snafu.chars().nth(reduce_char_idx).unwrap() == '=' {
//             reduce_char_idx += 1;
//         } else {
//             break;
//         }
//     }

//     let reduced_char = match snafu.chars().nth(reduce_char_idx).unwrap() {
//         '2' => '1',
//         '1' => '0',
//         '0' => '-',
//         '-' => '=',
//         _ => panic!(),
//     };

//     // println!("original {snafu}");
//     // println!("reduced_char: idx: {reduce_char_idx} char: {reduced_char}");

//     let prefix: &str = &snafu[0..reduce_char_idx];
//     let post_fix: Option<&str> = if reduce_char_idx < snafu.len() - 1 {
//         // println!("postfix: {:?}", &snafu[reduce_char_idx+1..]);
//         Some(&snafu[reduce_char_idx + 1..])
//     } else {
//         None
//     };

//     if let Some(post) = post_fix {
//         let mut increase_post: String = String::new();
//         (0..post.len()).for_each(|n| increase_post.push('2'));

//         format!("{prefix}{reduced_char}{increase_post}")
//     } else {
//         format!("{prefix}{reduced_char}")
//     }
// }

// // returns the snafu space we will use
// fn calculate_required_digits(decimal: i64) -> Vec<i64> {
//     // calculate the snafu digits we will use
//     let mut snafu_space: Vec<i64> = vec![];
//     let mut space_accum: i64 = 0;
//     for sn in &snafu_index() {
//         // println!("stretcher: {space_accum}");
//         if space_accum + 2 * sn <= decimal {
//             space_accum += 2 * sn;
//             snafu_space.push(*sn);
//         } else {
//             snafu_space.push(*sn);
//             break;
//         }
//         // e.g. 107
//         // (2 * 1) == 2,            <107
//         // 2 + 2 * 5 == 12,         <107
//         // 12 + 2 * 25 == 62,       <107
//         // 62 + 2 * 125 == 312,     >107 break
//         // snafu space == [125,25,5,1]
//     }
//     snafu_space.reverse();
//     snafu_space
// }


#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = r#"1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
"#;

    #[test]
    fn test() {
        assert_eq!(solve(EXAMPLE_INPUT), "2=-1=0");
    }
}
