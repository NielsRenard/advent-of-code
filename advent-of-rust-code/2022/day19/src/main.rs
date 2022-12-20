#![feature(result_option_inspect, string_extend_from_within)]
#![allow(dead_code)]

use std::collections::VecDeque;

use itertools::Itertools;
use regex::Regex;

#[macro_use]
extern crate measure_time;

#[doc(hidden)]
fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/19.input").unwrap()
}

#[doc(hidden)]
fn main() {
    env_logger::init();
    print_time!("execution");
    let answer_1 = solve_part_1(&input_from_file());
    println!("part 1: {answer_1}");
    let answer_2 = solve_part_2(&input_from_file());
    println!("part 2: {answer_2}");
}

fn solve_part_1(input: &str) -> usize {
    let blueprints = parse_input(input);
    debug!("{:?}", blueprints);

    let mut best_geodes: Vec<usize> = vec![];

    for blueprint in &blueprints {
        let geodes: usize = work(blueprint, 24);
        best_geodes.push(geodes);
    }

    best_geodes
        .iter()
        .enumerate()
        .map(|(i, geodes)| (i + 1) * geodes)
        .sum()
}

fn solve_part_2(input: &str) -> usize {
    let blueprints = parse_input(input);
    debug!("{:?}", blueprints);

    let mut best_geodes: Vec<usize> = vec![];

    for blueprint in blueprints.iter().take(3) {
        let geodes: usize = work(blueprint, 32);
        best_geodes.push(geodes);
    }

    best_geodes.iter().product()
}

struct State {
    ores: [usize; 4],
    bots: [usize; 4],
    minutes: usize,
}

fn work(blueprint: &[[usize; 4]; 4], total_minutes: usize) -> usize {
    let mut queue = VecDeque::new();
    queue.push_back(State {
        ores: [0, 0, 0, 0],
        bots: [1, 0, 0, 0],
        minutes: 0,
    });

    let mut best_geodes: usize = 0;

    while let Some(State {
        ores,
        bots,
        minutes,
    }) = queue.pop_front()
    {
        let mut max_needed = [usize::MAX; 4];
        (0..3).for_each(|i| {
            // if a robot costs x clay, and we have x clay-collecting bots
            // we don't need to build any more clay-collecting bots.

            // so we check each robot in our blueprint and which needs the most
            // resources, then store those highest numbers in max_needed.
            // what you end up with is a list that lets you know when
            // to stop building bots of a certain type
            max_needed[i] = blueprint.iter().map(|cost| cost[i]).max().unwrap();
        });

        for i in 0..blueprint.len() {
            if bots[i] == max_needed[i] {
                // as described above, we have enough bots, producing resource 'i'
                // and no longer need to build new ones.
                // By returning early, this branch will get pruned.
                continue;
            }
            // each bot
            let costs = &blueprint[i]; // cost for this particular bot
            let time_until_can_build = (0..3) // 0..3 because: ore, clay, obsidian
                .map(|resource_type| {
                    if costs[resource_type] <= ores[resource_type] {
                        // we have enough of the needed resource to build the robot right away
                        0
                    } else if bots[resource_type] == 0 {
                        // we currently have no robots of this type, it will take a minute at least
                        total_minutes + 1
                    } else {
                        // the cost to build a robot of this type -
                        // the amount of this resource we already have
                        // divided by the amount of bots collecting this resource
                        (blueprint[i][resource_type] - ores[resource_type] + bots[resource_type]
                            - 1)
                            / bots[resource_type]
                    }
                })
                .max()
                .unwrap();


            // prune branch if we can't build the robot before the
            // clock runs out
            let new_minutes = minutes + time_until_can_build + 1;
            if new_minutes >= total_minutes {
                continue;
            }

            // gather ores with previously available bots
            let mut new_ores = [0; 4];
            for idx in 0..bots.len() {
                new_ores[idx] = ores[idx] + bots[idx] * (time_until_can_build + 1) - costs[idx];
            }

            // increase bot type for the bot we just built
            let mut new_bots = bots;
            new_bots[i] += 1;

            // this checks what our score would be, if we built a
            // geode bot for every remaining turn (doesn't matter if
            // it's possible). prune this branch if we can't possibly
            // catch up to our current best heuristic.
            let remaining_time = total_minutes - new_minutes;
            if (remaining_time - 1) * remaining_time / 2
                + new_ores[3]
                + remaining_time * new_bots[3]
                < best_geodes
            {
                continue;
            }

            queue.push_back(State {
                ores: new_ores,
                bots: new_bots,
                minutes: new_minutes,
            })
        }

        let geodes = ores[3] + bots[3] * (total_minutes - minutes);
        best_geodes = geodes.max(best_geodes);
    }
    best_geodes
}

fn parse_input(input: &str) -> Vec<[[usize; 4]; 4]> {
    let re = Regex::new(r"\d+").unwrap();
    input
        .lines()
        .map(|l| {
            let cap: Vec<usize> = re
                .captures_iter(l)
                .map(|cs| {
                    cs.iter()
                        .flatten()
                        .flat_map(|c| c.as_str().parse::<usize>().ok())
                        .collect::<Vec<_>>()
                })
                .concat();

            let ore: [usize; 4] = [cap[1], 0, 0, 0];
            let clay: [usize; 4] = [cap[2], 0, 0, 0];
            let obsidian: [usize; 4] = [cap[3], cap[4], 0, 0];
            let geode: [usize; 4] = [cap[5], 0, cap[6], 0];
            [ore, clay, obsidian, geode]
        })
        .collect()
}

#[cfg(test)]
mod tests {

    use super::*;
    //
    const EXAMPLE_INPUT_1: &str = r#"Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian."#;

    const EXAMPLE_INPUT_2: &str = r#"Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"#;

    // #[test]
    // pub fn test() {
    //     env_logger::init();
    //     assert_eq!(solve_part_1(&[EXAMPLE_INPUT_1, EXAMPLE_INPUT_2].join("\n")), 33);
    // }

    #[test]
    pub fn test_two() {
        env_logger::init();
        assert_eq!(solve_part_2(EXAMPLE_INPUT_2), 62);
    }
}
