#![allow(dead_code, unused_mut, unused_variables, unused_imports)]
#![feature(iter_advance_by, iter_zip, destructuring_assignment, map_try_insert)]

use std::{num, collections::HashSet};
use itertools::Itertools;

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2021/19.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {}", answer_1);
    // let answer_2 = solve_part_two(input_from_file().lines().next().unwrap());
    // println!("part 2: {}", answer_2);
    // execution took 2.49ms
}

type Point = (i32, i32, i32);

fn solve_part_one(input: &str) -> i64 {
    let scanners: Vec<Vec<Point>> = parse(&input);
    let distances_0 : HashSet<_> = HashSet::from_iter(distances_between_beacons(&scanners[0]).iter().cloned());
    let distances_1 : HashSet<_> = HashSet::from_iter(distances_between_beacons(&scanners[1]).iter().cloned());
    let intersection_0_1_ = distances_0.intersection(&distances_1);
    println!("{:?}", intersection_0_1_);
    // pretty_print(scanners);
    1
}

fn distances_between_beacons(beacons: &Vec<Point>) -> Vec<Point> {
    let mut distance_vectors: Vec<_> = vec![];
    for (i, beacon@(x,y,z)) in beacons.iter().enumerate() {
        for (j, other_beacon@(p,q,r)) in beacons.iter().enumerate() {
            if i == j { continue }
            // let difference_of_all_axes = ((x-p)^2 + (y-q)^2 + (z-r)^2).abs();
            // let distance = (((x-p)^2 + (y-q)^2 + (z-r)^2) as f64).abs().sqrt();
            let distance_vec = (x-p, y-q, z-r);
            let rotations = rotate_one(distance_vec);
            distance_vectors.push(distance_vec);
            // println!("i:{}: {:?} to j: {}: {:?} = {:?}", i, beacon, j, other_beacon, distance_vec);
        }
    }
    // println!("{:?}", distances.len());
    distance_vectors
}

fn rotate_one(beacon@(x,y,z): Point) -> Vec<Point> {
    let xzy = (x,z,y);
    let yzx = (y,z,x);
    let yxz = (y,x,z);
    let zyx = (z,y,x);
    let zxy = (z,x,y);
    println!("turning! "); 
    println!("init: {:?}", beacon);
    println!("{:?},{:?},{:?},{:?},{:?}", xzy, yzx, yxz, zyx, zxy);
    println!(); 
    vec![beacon, xzy, yzx, yxz, zyx, zxy]
}


fn parse(input: &str) -> Vec<Vec<Point>> {
    let scanners: Vec<Vec<&str>> = input
        .lines()
        .filter(|l| !l.starts_with("---"))
        .collect::<Vec<_>>()
        .split(|s| *s == "")
        .map(|arr| arr.to_owned())
        .collect();

    let points: Vec<Vec<Point>> = scanners
        .iter()
        .map(|vec| {
            vec.iter()
                .map(|s| s.split(',').collect_vec())
                .map(|numbers| {
                    numbers
                        .into_iter()
                        .map(|num| i32::from_str_radix(num, 10).unwrap())
                        .collect::<Vec<i32>>()
                })
                .map(|vec| (vec[0], vec[1], vec[2]))
                .collect()
        })
        .collect();
    points
}

fn pretty_print(scanners: Vec<Vec<Point>>) {
    scanners.iter().enumerate().for_each(|(i, beacons)| {
        println!("--- scanner {:?} --- ", i);
        beacons.iter().for_each(|beacon| println!("{:?}", beacon));
        println!();
    });
}



#[cfg(test)]
mod tests {

    use super::*;
    const EXAMPLE_INPUT: &str = "--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14";

    #[test]
    fn test_1() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 79);
    }
}
