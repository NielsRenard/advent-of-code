#![feature(slice_flatten, generic_arg_infer)]

pub use std::collections::HashMap;

#[macro_use]
extern crate measure_time;

fn main() {}

/// takes an array with pairs of a sensor and its closest beacon
///
/// 1. for each sensor, check how many steps it takes to its nearest beacon, let's call this 'sensor_range'
/// 2. if a pos x is within range of a sensor, there can NOT be a beacon in that x position
fn solve_part_one_test(input: [[[i64; 2]; 2]; 14]) -> usize {
    let mut known_coords: Vec<&[i64; 2]> = input.iter().flatten().collect();
    println!("known_coords len: {}", known_coords.len());
    known_coords.sort();
    known_coords.dedup();
    println!("dedupped known_coords len: {},", known_coords.len());

    let x_min = known_coords
        .iter()
        .map(|[x, _]| x)
        .min()
        .unwrap()
        .to_owned();
    let x_max = known_coords
        .iter()
        .map(|[x, _]| x)
        .max()
        .unwrap()
        .to_owned();

    //1.
    let mut sensor_ranges: [[i64; 3]; 14] = [[0; 3]; 14];
    for (i, [[sx, sy], [bx, by]]) in input.into_iter().enumerate() {
        sensor_ranges[i] = [sx, sy, manhattan_distance((sx, sy), (bx, by))];
    }

    //2.
    let mut no_beacon_count = 0;
    for x in x_min..=x_max {
        if known_coords.contains(&&[x, 10]) {
            continue;
        }
        let mut any_sensor_in_range = false;
        for [s_x, s_y, range] in sensor_ranges {
            let distance = manhattan_distance((x, 10), (s_x, s_y));
            if range < distance {
            } else {
                print!("S{s_x},{s_y} ");
                any_sensor_in_range = true;
                break;
            }
        }
        if any_sensor_in_range {
            no_beacon_count += 1;
        }
        println!();
    }
    no_beacon_count
}

/// takes an array with pairs of a sensor and its closest beacon
///
/// 1. for each sensor, check how many steps it takes to its nearest beacon, let's call this 'sensor_range'
/// 2. if a pos x is within range of a sensor, there can NOT be a beacon in that x position
fn solve_part_two_test(input: [[[i64; 2]; 2]; 14], max: u64) -> (u64, u64) {
    let mut known_coords: Vec<&[i64; 2]> = input.iter().flatten().collect();
    known_coords.sort();
    known_coords.dedup();

    //1.
    let mut sensor_ranges: [[i64; 3]; 14] = [[0; 3]; 14];
    for (i, [[sx, sy], [bx, by]]) in input.into_iter().enumerate() {
        sensor_ranges[i] = [sx, sy, manhattan_distance((sx, sy), (bx, by))];
    }

    sensor_ranges
        .sort_by(|[a_x, a_y, a_r], [b_x, b_y, b_r]| (a_x - (a_r - a_y)).cmp(&(b_x - (b_r - b_y))));

    let min = 0;
    let max = max;

    println!("min {min}, max {max}");

    // naive approach, won't work on real input
    for y in min..=max {
        for x in min..=max {
            if known_coords.contains(&&[x as i64, y as i64]) {
                continue;
            }
            let mut any_sensor_in_range = false;

            for [s_x, s_y, range] in sensor_ranges {
                let distance = manhattan_distance((x as i64, y as i64), (s_x, s_y));
                if range < distance {
                    // sensor can't reach here
                } else {
                    any_sensor_in_range = true;
                    break;
                }
            }
            if any_sensor_in_range {
                // not interested
            } else {
                println!("x{x},y{y}");
                return (x, y);
            }
        }
    }
    (0, 0)
}

/// takes an array with pairs of a sensor and its closest beacon
///
/// 1. for each sensor, check how many steps it takes to its nearest beacon, let's call this 'sensor_range'
/// 2. if a pos x is within range of a sensor, there can NOT be a beacon in that x position
fn solve_part_one_real(input: [[[i64; 2]; 2]; 29], y: u64) -> usize {
    let mut known_coords: Vec<&[i64; 2]> = input.iter().flatten().collect();
    known_coords.sort();
    known_coords.dedup();

    //1.
    let mut sensor_ranges: [[i64; 3]; 29] = [[0; 3]; 29];
    for (i, [[sx, sy], [bx, by]]) in input.into_iter().enumerate() {
        sensor_ranges[i] = [sx, sy, manhattan_distance((sx, sy), (bx, by))];
    }

    /*
    Determining min_x and max_x

    The sensor who's range can reach the furthest x where y == 0 is
    determined by subtracting its height from its range, and deducting that result from it's x
    x - (r - y)

    01.......................
    .23......................
    ..45.....................
    ...67....................
    ....89.................y0
    .........................
    012......................
    ..345....................
    ....678..................
    ......9................y0.
    .........................
    0123.....................
    ...456...................
    .....789...............y0
    .........................
    01234....................
    ....56789..............y0
    .........................
    0123456789.............y0
    */

    sensor_ranges
        .sort_by(|[a_x, a_y, a_r], [b_x, b_y, b_r]| (a_x - (a_r - a_y)).cmp(&(b_x - (b_r - b_y))));

    let x_min = sensor_ranges[0][0] - sensor_ranges[0][2];
    let x_max = sensor_ranges.last().unwrap()[0] + sensor_ranges.last().unwrap()[2];

    println!("x_min {x_min}, x_max {x_max}");

    //2.
    let mut no_beacon_count = 0;
    for x in x_min..=x_max {
        if known_coords.contains(&&[x, y as i64]) {
            println!("{x},{y}");
            continue;
        }
        let mut any_sensor_in_range = false;

        for [s_x, s_y, range] in sensor_ranges {
            let distance = manhattan_distance((x, y as i64), (s_x, s_y));
            if range < distance {
                // sensor can't reach here
            } else {
                any_sensor_in_range = true;
                break;
            }
        }
        if any_sensor_in_range {
            no_beacon_count += 1;
        }
    }
    no_beacon_count
}

/// takes an array with pairs of a sensor and its closest beacon
fn solve_part_two_real(input: [[[i64; 2]; 2]; 29]) -> i64 {
    let mut sensor_ranges: [[i64; 3]; 29] = [[0; 3]; 29];
    for (i, [[sx, sy], [bx, by]]) in input.into_iter().enumerate() {
        sensor_ranges[i] = [sx, sy, manhattan_distance((sx, sy), (bx, by))];
    }

    let empty_pos = find_unscannable_spot(sensor_ranges);
    let answer = (empty_pos.0 * 4_000_000) + empty_pos.1;
    println!("{answer}");
    answer
}

fn within_sensor_range((x, y): (i64, i64), sensors: [[i64; 3]; 29]) -> bool {
    for &[sx, sy, range] in sensors.iter() {
        let dist = manhattan_distance((x, y), (sx, sy));
        if dist < range + 1 {
            return true;
        }
    }
    false
}

fn find_unscannable_spot(sensors: [[i64; 3]; 29]) -> (i64, i64) {
    for &[sx, sy, r] in sensors.iter() {
        // it has to be somewhere range + 1
        for a in 0..=r + 1 {
            let b = r + 1 - a;
            for (dx, dy) in [(a, b), (a, -b), (-a, b), (-a, -b)] {
                let x = sx + dx;
                let y = sy + dy;
                if (0..=4_000_000).contains(&x)
                    && (0..=4_000_000).contains(&y)
                    && !within_sensor_range((x, y), sensors)
                {
                    return (x, y);
                }
            }
        }
    }
    panic!()
}

fn manhattan_distance(point1: (i64, i64), point2: (i64, i64)) -> i64 {
    let dx = point1.0 - point2.0;
    let dy = point1.1 - point2.1;
    dx.abs() + dy.abs()
}

#[cfg(test)]
mod tests {

    /// ...3..
    /// ..323..
    /// .32123.
    /// 321S123
    /// .32123
    /// ..323..
    /// ...3...

    const REAL_INPUT: [[[i64; 2]; 2]; 29] = [
        [[3797530, 3451192], [3316341, 3328308]],
        [[3779164, 33938], [4608350, 708806]],
        [[1331810, 3260896], [2075597, 3280016]],
        [[393374, 696899], [2021690, 453306]],
        [[2928048, 923094], [2021690, 453306]],
        [[2386726, 3645023], [2075597, 3280016]],
        [[1900159, 2381031], [1649961, 2000000]],
        [[2601378, 2979844], [2218962, 2701963]],
        [[2254818, 32199], [2021690, 453306]],
        [[2689643, 375840], [2021690, 453306]],
        [[909141, 2842547], [2218962, 2701963]],
        [[3915731, 2454320], [4268501, 1853073]],
        [[1693574, 1344104], [1649961, 2000000]],
        [[1760260, 3297662], [2075597, 3280016]],
        [[1909567, 3990737], [2075597, 3280016]],
        [[2097863, 3179766], [2075597, 3280016]],
        [[3100489, 3623847], [3104748, 4102403]],
        [[2746023, 2432826], [2218962, 2701963]],
        [[3031245, 3031354], [3316341, 3328308]],
        [[277094, 1999350], [1649961, 2000000]],
        [[1763269, 126349], [2021690, 453306]],
        [[3287624, 2695420], [3316341, 3328308]],
        [[2371102, 1745103], [1649961, 2000000]],
        [[3553438, 1563379], [4268501, 1853073]],
        [[1529129, 2735122], [2218962, 2701963]],
        [[2826220, 3958350], [3104748, 4102403]],
        [[3999334, 3912693], [3104748, 4102403]],
        [[240430, 3829436], [-742036, 3963149]],
        [[3455748, 3814861], [3316341, 3328308]],
    ];

    use super::*;
    const EXAMPLE_INPUT: [[[i64; 2]; 2]; 14] = [
        [[2, 18], [-2, 15]],
        [[9, 16], [10, 16]],
        [[13, 2], [15, 3]],
        [[12, 14], [10, 16]],
        [[10, 20], [10, 16]],
        [[14, 17], [10, 16]],
        [[8, 7], [2, 10]],
        [[2, 0], [2, 10]],
        [[0, 11], [2, 10]],
        [[20, 14], [25, 17]],
        [[17, 20], [21, 22]],
        [[16, 7], [15, 3]],
        [[14, 3], [15, 3]],
        [[20, 1], [15, 3]],
    ];

    #[test]
    fn test() {
        assert_eq!(solve_part_one_test(EXAMPLE_INPUT), 26);
    }

    #[test]
    fn real() {
        print_time!("execution");
        let range = vec![2000000];
        for y in range.iter() {
            assert_eq!(solve_part_one_real(REAL_INPUT, *y), 5511201);
        }
    }

    #[test]
    fn real_part_two() {
        print_time!("execution");
        assert_eq!(solve_part_two_real(REAL_INPUT), 11318723411840);
    }
}
