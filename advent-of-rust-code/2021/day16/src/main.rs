#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use std::ops::Add;

use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/16.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(input_from_file().lines().next().unwrap());
    println!("part 1: {}", answer_1);
    let answer_2 = solve_part_two(input_from_file().lines().next().unwrap());
    println!("part 2: {}", answer_2);
    //execution took 2.49ms

    /*
      woops! had a silly bug, too high 1997493262715211, kept looking for integer overflows
      finally I found that I forgot to strip off the 1/0 "last group identifier" from each chunk, for literal values
    */
}

fn binary_string_to_decimal(binary_string: &str) -> i64 {
    i64::from_str_radix(binary_string, 2).unwrap()
}

fn solve_part_one(input: &str) -> i64 {
    /* Refresher, HEX to BIN:
          D    2    F    E    2    8
         13    2   15   12    2    8 // if necessary, convert to dec value
       8421 8421 8421 8421 8421 8421 // use 8,4,2,1 bits to sum up value
       1101 0010 1111 1100 0010 1000 // mark bits used, this is your binary
    */

    /*
      {:08b} thing: https://doc.rust-lang.org/std/fmt/index.html
    */
    let bytes: Vec<u8> = hex::decode(input).unwrap();
    let binary: String = bytes.iter().map(|byte| format!("{:08b}", byte)).join("");
    let ParsePacketReturn {
        remaining_transmission,
        version_sum,
        value_sum,
    } = parse_packets_recursive(&binary);
    assert!(remaining_transmission.chars().all(|c| c == '0'));
    version_sum
}

fn solve_part_two(input: &str) -> i64 {
    let binary = hex_to_binary_string(input.to_string());
    let ParsePacketReturn {
        remaining_transmission,
        version_sum,
        value_sum,
    } = parse_packets_recursive(&binary);
    assert!(remaining_transmission.chars().all(|c| c == '0'));
    value_sum
}

#[derive(Debug)]
struct ParsePacketReturn {
    remaining_transmission: String,
    version_sum: i64,
    value_sum: i64,
}

fn parse_packets_recursive(binary: &str) -> ParsePacketReturn {
    let version = binary_string_to_decimal(&binary[0..3]);
    let mut version_sum = version;
    let type_id = binary_string_to_decimal(&binary[3..6]);
    let packet_type = if type_id == 4 { "literal" } else { "operator" };

    let mut packet_end_idx;
    let remaining: &Vec<char> = &binary.chars().into_iter().skip(6).collect_vec();
    let mut remainder = binary.to_owned();

    let mut sub_packets_start_pos = 0;
    let (length_type, subpacket_length_or_count) = match packet_type {
        length_type @ "literal" => (length_type, 0),
        length_type @ "operator" => {
            let length_bit = &binary[6..7];
            if length_bit == "0" {
                // header + len bit + (type 0 =) subpacket length, 6+1+15 = 22 bits
                sub_packets_start_pos = 6 + 1 + 15;
                let subpacket_length =
                    binary_string_to_decimal(&binary[7..sub_packets_start_pos]) as usize;
                ("total_length_of_sub_packets", subpacket_length)
            } else {
                assert_eq!(length_bit, "1");
                // header + length bit, 6+1+11 = 18 bits
                sub_packets_start_pos = 6 + 1 + 11;
                let number_of_subpackets =
                    binary_string_to_decimal(&binary[7..sub_packets_start_pos]) as usize;
                ("number_of_sub_packets", number_of_subpackets)
            }
        }
        _ => panic!(),
    };

    /*
    actual work of getting subpackets starts here:
     */
    let mut literal_value = 0;
    let mut subpacket_values: Vec<i64> = Vec::new();
    match packet_type {
        "literal" => {
            let mut literal_binary: Vec<&str> = Vec::new();
            /*
               1. keep taking chunks of 5 bits,
               2. if the first bit of a chunk is 0, this is the final chunk
               3. these chunks form your literal value and packet is now finished
            */

            // (header + version (6), 6 + last_chunk_indicator + 4 bits)
            let (mut chunk_start_idx, mut chunck_end_idx) = (6, 10);
            loop {
                let chunk = &binary[chunk_start_idx..=chunck_end_idx];
                literal_binary.push(&chunk[1..]);
                if chunk.starts_with('0') {
                    break;
                }
                (chunk_start_idx, chunck_end_idx) = (chunk_start_idx + 5, chunck_end_idx + 5);
                // This could help you understand chunk_end_idx and packet_end_idx relationship
                // hex: D2FE28
                //
                // 5s  ....1....2....3....5....
                // idx 0.........10........20..
                // bin 110100101111111000101000
                //     VERTYPCHUNKCHUNKCHUNK...
            }
            literal_value = binary_string_to_decimal(&literal_binary.join(""));
            packet_end_idx = chunck_end_idx + 1; // +1 because include last char of chunk
        }
        "operator" => {
            match length_type {
                "total_length_of_sub_packets" => {
                    let subpackets = &binary[sub_packets_start_pos
                        ..(sub_packets_start_pos + subpacket_length_or_count)];
                    let mut subpackets_remainder: String = subpackets.to_owned();
                    let mut sub_version_sum = 0;
                    loop {
                        // println!("parsing {} bits of subpackets... ", subpacket_length_or_count);
                        // println!();
                        let parse_packets_return: ParsePacketReturn =
                            parse_packets_recursive(&subpackets_remainder);
                        subpackets_remainder = parse_packets_return.remaining_transmission.clone();
                        subpacket_values.push(parse_packets_return.value_sum);
                        sub_version_sum += parse_packets_return.version_sum;
                        if subpackets_remainder.chars().all(|c| c == '0') {
                            break;
                        }
                    }
                    version_sum += sub_version_sum;
                    packet_end_idx = sub_packets_start_pos + subpacket_length_or_count;
                }
                "number_of_sub_packets" => {
                    // println!("parsing {} subpacket(s)... ", subpacket_length_or_count);
                    // println!();
                    let subpackets = &binary[sub_packets_start_pos..];
                    let total_remain_len = subpackets.len();
                    let mut count = 0;
                    let mut sub_version_sum = 0;
                    let mut subpackets_remainder: String = subpackets.to_owned();
                    loop {
                        if count == subpacket_length_or_count {
                            break;
                        }
                        let parse_packets_return: ParsePacketReturn =
                            parse_packets_recursive(&subpackets_remainder);
                        subpackets_remainder = parse_packets_return.remaining_transmission.clone();
                        sub_version_sum += parse_packets_return.version_sum;
                        subpacket_values.push(parse_packets_return.value_sum);
                        count += 1;
                    }
                    version_sum += sub_version_sum;
                    packet_end_idx =
                        sub_packets_start_pos + (total_remain_len - subpackets_remainder.len())
                }
                _ => panic!(),
            };
        }
        _ => panic!(),
    }

    /*
      perform calculations for part 2
    */
    let mut value_sum: i64 = 0;
    if packet_type == "operator" {
        assert_eq!(value_sum, 0);
        value_sum += match type_id {
            0 => subpacket_values.iter().sum::<i64>(),
            1 => subpacket_values.iter().product::<i64>(),
            2 => *subpacket_values.iter().min().unwrap(),
            3 => *subpacket_values.iter().max().unwrap(),
            5 => {
                if subpacket_values[0] > subpacket_values[1] {
                    1
                } else {
                    0
                }
            }
            6 => {
                if subpacket_values[0] < subpacket_values[1] {
                    1
                } else {
                    0
                }
            }
            7 => {
                if subpacket_values[0] == subpacket_values[1] {
                    1
                } else {
                    0
                }
            }
            _ => panic!(),
        }
    } else if packet_type == "literal" {
        value_sum = literal_value;
    }

    ParsePacketReturn {
        remaining_transmission: binary[packet_end_idx..].to_string(),
        version_sum,
        value_sum,
    }
}

fn hex_to_binary_string(hex_string: String) -> String {
    hex_string
        .chars()
        .into_iter()
        .map(|c| match c {
            'A' | 'a' => "1010",
            'B' | 'b' => "1011",
            'C' | 'c' => "1100",
            'D' | 'd' => "1101",
            'E' | 'e' => "1110",
            'F' | 'f' => "1111",
            '0' => "0000",
            '1' => "0001",
            '2' => "0010",
            '3' => "0011",
            '4' => "0100",
            '5' => "0101",
            '6' => "0110",
            '7' => "0111",
            '8' => "1000",
            '9' => "1001",
            _ => panic!(),
        })
        .collect()
}

#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT_D2FE28: &str = "D2FE28";

    const EXAMPLE_INPUT_8A004A801A8002F478: &str = "8A004A801A8002F478";

    const EXAMPLE_INPUT_C0015000016115A2E0802F182340: &str = "C0015000016115A2E0802F182340";

    const EXAMPLE_INPUT_A0016C880162017C3686B18A3D4780: &str = "A0016C880162017C3686B18A3D4780";

    #[test]
    fn test_1_d2fe28() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT_D2FE28), 6);
    }

    #[test]
    fn test_1_38006f45291200() {
        assert_eq!(solve_part_one("38006f45291200"), 9);
    }

    #[test]
    fn test_1_ee00d40c823060() {
        assert_eq!(solve_part_one("ee00d40c823060"), 14);
    }

    #[test]
    fn test_1_8a004a801a8002f478() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT_8A004A801A8002F478), 16);
    }

    #[test]
    fn test_1_c0015000016115a2e0802f182340() {
        assert_eq!(
            solve_part_one(EXAMPLE_INPUT_C0015000016115A2E0802F182340),
            23
        );
    }

    #[test]
    fn test_1_620080001611562c8802118e34() {
        assert_eq!(solve_part_one("620080001611562c8802118e34"), 12);
    }

    #[test]
    fn test_1_a0016c880162017c3686b18a3d4780() {
        assert_eq!(
            solve_part_one(EXAMPLE_INPUT_A0016C880162017C3686B18A3D4780),
            31
        );
    }

    #[test]
    fn test_2_sum() {
        assert_eq!(solve_part_two("C200B40A82"), 3);
    }

    #[test]
    fn test_2_product() {
        assert_eq!(solve_part_two("04005AC33890"), 54);
    }

    #[test]
    fn test_2_minimum() {
        assert_eq!(solve_part_two("880086C3E88112"), 7);
    }

    #[test]
    fn test_2_maximum() {
        assert_eq!(solve_part_two("CE00C43D881120"), 9);
    }

    #[test]
    fn test_2_greater_than() {
        assert_eq!(solve_part_two("F600BC2D8F"), 0);
    }

    #[test]
    fn test_2_less_than() {
        assert_eq!(solve_part_two("D8005AC2A8F0"), 1);
    }

    #[test]
    fn test_2_equal() {
        assert_eq!(solve_part_two("9C005AC2F8F0"), 0);
    }

    #[test]
    fn test_2_complex() {
        // 9C0141080250320F1802104A08 produces 1, because 1 + 3 = 2 * 2.
        assert_eq!(solve_part_two("9C0141080250320F1802104A08"), 1);
    }

    #[test]
    fn test_2_sum_reddit() {
        // "That packet is (sum (sum 1 (sum 1)) (sum 1)), so the answer you get should be 3."
        assert_eq!(solve_part_two("a0017e00028b034400d8180002e408"), 3);
    }

    #[test]
    fn test_2_8a004a801a8002f478() {
        // from part 1
        assert_eq!(solve_part_two(EXAMPLE_INPUT_8A004A801A8002F478), 15);
    }

    #[test]
    fn test_2_overflow_david_yue_tool() {
        // https://davidyue.live/aoc/app/packetcode.html
        // 100000 * 100000 = 10000000000
        // somehow my 100000's are becoming 18635584
        // yup, It was in my literal checking code, was not taking off the "last group identifier" from each chunk
        assert_eq!(solve_part_two("26008C8E2DA0191C5B400"), 10000000000);
    }

    #[test]
    fn test_2_overflow_reddit_2() {
        // literal 5000000000
        assert_eq!(solve_part_two("3232D42BF9400"), 5000000000);
    }
}
