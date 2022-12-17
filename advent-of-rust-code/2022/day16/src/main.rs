#![allow(dead_code, unused_mut, unused_variables)]
#![feature(result_option_inspect, string_extend_from_within)]

use dot_graph::{Edge, Graph, Node};
pub use std::collections::HashMap;
use std::{collections::HashSet, fs};

#[macro_use]
extern crate measure_time;

fn input_from_file() -> String {
    std::fs::read_to_string("../../data/2022/16.input").unwrap()
}

fn main() {
    print_time!("execution");
    let answer_1 = solve_part_one(&input_from_file());
    println!("part 1: {answer_1}");
    // part 1: 1647
    // execution took 1s 113ms
}

type AdjacencyList = HashMap<String, Vec<String>>;

/*

- 30 minutes before the volcano erupts
- flow rate is in pressure per minute
- one minute to open a single valve
- one minute to follow any tunnel, to the next valve

  "What is the most pressure you could release?"

   solved using dfs with a heuristic and cache
*/

fn solve_part_one(input: &str) -> i32 {
    let (adjacency_list, valves): (HashMap<String, Vec<String>>, Vec<Valve>) = parse_input(input);
    print_graph(&(adjacency_list.clone(), valves.clone()));

    let remaining_time: i32 = 30;
    let mut path: Vec<String> = vec![];

    let mut cache: HashMap<(String, Vec<String>, i32), i32> = HashMap::new();
    let answer: i32 = recur(
        "AA".to_string(),
        &mut path,
        &adjacency_list,
        &valves,
        remaining_time,
        &mut cache,
    );
    answer
}

fn recur(
    cur_pos: String,
    path: &mut Vec<String>,
    graph: &HashMap<String, Vec<String>>,
    valves: &Vec<Valve>,
    remaining_time: i32,
    cache: &mut HashMap<(String, Vec<String>, i32), i32>,
) -> i32 {
    if remaining_time <= 0 {
        return 0;
    }

    if let Some(&answer) = cache.get(&(cur_pos.to_owned(), path.clone(), remaining_time)) {
        return answer;
    }

    // this will be our answer, we replace it whenever we find a
    // better outcome
    let mut heuristic = 0;

    let cur_valve = valves.iter().find(|v| v.id == cur_pos).unwrap();
    let non_zero_flow_rate = cur_valve.flow_rate > 0;
    let been_here_before = path.contains(&cur_pos);

    // case 1: we haven't been here, and we DO open the valve
    if non_zero_flow_rate && !been_here_before {
        for child in graph.get(&cur_pos).unwrap() {
            // push current position to the child nodes path, so recursive calls don't
            // repeat visit
            path.push(cur_pos.to_owned());
            // time_remaining minus - 2 minutes because we moved AND
            // open this valve
            let recur_answer = recur(
                child.to_string(),
                path,
                graph,
                valves,
                remaining_time - 2,
                cache,
            );
            // 1. multiply the *remaining* minutes by the now
            // increased flow rate (current minute does not count, so
            // minus 1)
            // 2. add whatever came back from the recursive call to
            // our new flowrate.
            // if the new value is better, this means this path was
            // more succesful, so replace our heuristic value
            heuristic = heuristic.max(recur_answer + cur_valve.flow_rate * (remaining_time - 1));
            // take current pos out of the path again, so other paths
            // can visit it
            // println!("path {path:?}");
            path.pop();
        }
    }
    else {
    // case 2: either the flow rate is 0, or we've already been
    // here (at a differen time).
    // We are NOT opening a valve, so only 1 minute deducation
    for tunnel in graph.get(&cur_pos).unwrap().iter() {
        let recur_answer = recur(
            tunnel.to_string(),
            path,
            graph,
            valves,
            remaining_time - 1,
            cache,
        );
        heuristic = heuristic.max(recur_answer);
    }
    }

    cache.insert((cur_pos, path.clone(), remaining_time), heuristic);
    heuristic
}

fn print_graph((adj_list, valves): &(AdjacencyList, Vec<Valve>)) {
    let mut graph = Graph::new("valves", dot_graph::Kind::Graph);
    for valve in valves {
        graph.add_node(Node::new(&valve.id).label(&format!("{}\n{}", valve.flow_rate, valve.id)));
    }
    for (node, tunnels) in adj_list.into_iter() {
        for tunnel in tunnels {
            graph.add_edge(Edge::new(&node, &tunnel, ""));
        }
    }
    let graph = graph.to_dot_string().unwrap();

    let mut lines: Vec<&str> = graph.lines().collect();
    let options: &str = "    layout=circo; overlap=scalexy; sep=\"0.1\";";
    let mut v = lines.split_off(1);
    lines.extend_from_slice(&[options]);
    lines.append(&mut v);
    let _ = fs::write("graph.dot", lines.join("\n"));
}

#[derive(Debug, Clone)]
struct Valve {
    id: String,
    flow_rate: i32,
    tunnels: Vec<String>,
}

fn parse_input(input: &str) -> (AdjacencyList, Vec<Valve>) {
    let dict: HashMap<String, usize> = HashMap::new();
    let valves = input
        .lines()
        .map(|l| {
            let (first, mut second) = l.split_once("; ").unwrap();
            let mut split = first.split_ascii_whitespace();
            let valve = split.nth(1).unwrap();
            let rate = split
                .nth(2)
                .and_then(|x| x.split('=').last())
                .and_then(|x| x.parse().ok())
                .unwrap();

            let second = second.replace(',', "");
            let tunnels: Vec<String> = second
                .split_ascii_whitespace()
                .skip(4)
                .map(|s| s.to_string())
                .collect();

            Valve {
                id: valve.to_string(),
                flow_rate: rate,
                tunnels,
            }
        })
        .collect::<Vec<Valve>>();

    let mut adjacency_list: HashMap<String, Vec<String>> = HashMap::new();
    for valve in valves.iter() {
        adjacency_list.insert(valve.id.clone(), valve.tunnels.clone());
    }

    (adjacency_list, valves)
}

#[cfg(test)]
mod tests {

    use super::*;

    const EXAMPLE_INPUT: &str = r#"Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"#;

    #[test]
    fn test() {
        assert_eq!(solve_part_one(EXAMPLE_INPUT), 1651);
    }
}
