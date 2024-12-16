use std::{collections::HashMap, fs::read_to_string};

use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};

fn step_stone(n: usize) -> Vec<usize> {
    if n == 0 {
        return vec![1];
    }
    if (n.ilog10() + 1) % 2 == 0 {
        let s = n.to_string();
        let (l, r) = s.split_at(s.len() / 2);
        return vec![l.parse().unwrap(), r.parse().unwrap()];
    }
    return vec![n * 2024];
}

fn step_stones(input: Vec<usize>) -> Vec<usize> {
    input
        .into_par_iter()
        .flat_map_iter(|x| step_stone(x))
        .collect()
}

fn take_n_steps(input: Vec<usize>, step_count: usize) -> Vec<usize> {
    let mut curr = input;

    for step in 0..step_count {
        println!("Step: {}", step);
        curr = step_stones(curr);
    }

    return curr;
}

fn take_n_steps_dp(input: Vec<usize>, step_count: usize) -> Vec<usize> {
    let mut dp: HashMap<usize, HashMap<usize, usize>> = HashMap::new();
    let mut curr = input;

    for step in 0..step_count {
        let remaining = step_count - step;
        println!("Step: {}", step);
        curr = step_stones(curr);
    }

    return curr;
}

fn step_stone_dp(
    n: usize,
    steps: usize,
    dp: &mut HashMap<usize, HashMap<usize, usize>>,
) -> usize {
    let cached_res = dp.get(&steps).and_then(|m| m.get(&n));
    if cached_res.is_some() {
        return *cached_res.unwrap();
    }
    if steps == 0 {
        return 1;
    }

    let res;
    if n == 0 {
        res = step_stone_dp(1, steps - 1, dp);
    } else if (n.ilog10() + 1) % 2 == 0 {
        let s = n.to_string();
        let (l, r) = s.split_at(s.len() / 2);
        let l_num = l.parse().unwrap();
        let l_res = step_stone_dp(l_num, steps - 1, dp);
        let r_num = r.parse().unwrap();
        let r_res = step_stone_dp(r_num, steps - 1, dp);
        res = l_res + r_res;
    } else {
        res = step_stone_dp(n * 2024, steps - 1, dp);
    }

    dp.entry(steps).or_insert(HashMap::new()).insert(n, res);

    return res;
}

fn parse(s: &str) -> Vec<usize> {
    s.trim()
        .split_whitespace()
        .map(|n| n.parse::<usize>().unwrap())
        .collect()
}

fn part_one() -> String {
    let s = read_to_string("input.txt").unwrap();
    take_n_steps(parse(&s), 25).len().to_string()
}

fn part_two() -> String {
    let s = read_to_string("input.txt").unwrap();
    let mut map = HashMap::new();
    parse(&s)
        .iter()
        .map(|r| step_stone_dp(*r, 75, &mut map))
        .sum::<usize>()
        .to_string()
}

fn main() {
    println!("{}", part_one());
    println!("{}", part_two());
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &'static str = r"125 17";

    #[test]
    fn example() {
        let i = parse(INPUT);
        let res = take_n_steps(i, 25);
        assert_eq!(res.len(), 55312);
    }
}
