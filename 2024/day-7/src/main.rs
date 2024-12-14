use std::{collections::VecDeque, fs::read_to_string};

fn search(nums: &Vec<usize>, target: usize) -> usize {
    let first = nums[0];
    let rest = nums
        .iter()
        .skip(1)
        .map(|n| n.to_owned())
        .collect::<VecDeque<usize>>();
    return search_helper(first, rest, target);
}

fn sum_sat_targets(v: Vec<(usize, Vec<usize>)>) -> usize {
    v.iter()
        .filter(|(target, nums)| search(nums, *target) > 0)
        .map(|(target, _)| target)
        .sum()
}

fn search2(nums: &Vec<usize>, target: usize) -> usize {
    let first = nums[0];
    let rest = nums
        .iter()
        .skip(1)
        .map(|n| n.to_owned())
        .collect::<VecDeque<usize>>();
    return search_helper2(first, rest, target);
}

fn sum_sat_targets2(v: Vec<(usize, Vec<usize>)>) -> usize {
    v.iter()
        .filter(|(target, nums)| search2(nums, *target) > 0)
        .map(|(target, _)| target)
        .sum()
}

fn part_one() -> String {
    let s = read_to_string("input.txt").unwrap();
    let input = parse(&s);
    sum_sat_targets(input).to_string()
}

fn part_two() -> String {
    let s = read_to_string("input.txt").unwrap();
    let input = parse(&s);
    sum_sat_targets2(input).to_string()
}

fn search_helper(acc: usize, mut rest: VecDeque<usize>, target: usize) -> usize {
    if acc == target && rest.is_empty() {
        return 1;
    }
    if acc > target {
        return 0;
    }
    let next_num = rest.pop_front();
    next_num.map_or(0, |n| {
        search_helper(n + acc, rest.clone(), target)
            + search_helper(n * acc, rest.clone(), target)
    })
}

fn search_helper2(acc: usize, mut rest: VecDeque<usize>, target: usize) -> usize {
    if acc == target && rest.is_empty() {
        return 1;
    }
    if acc > target {
        return 0;
    }
    let next_num = rest.pop_front();
    next_num.map_or(0, |n| {
        search_helper2(n + acc, rest.clone(), target)
            + search_helper2(n * acc, rest.clone(), target)
            + search_helper2(
                format!("{}{}", acc, n).parse::<usize>().unwrap(),
                rest.clone(),
                target,
            )
    })
}

fn parse(s: &str) -> Vec<(usize, Vec<usize>)> {
    s.trim()
        .lines()
        .map(|l| {
            let parts: Vec<_> = l.trim().split(":").collect();
            if let [target, nums] = parts.as_slice() {
                return (
                    target.parse::<usize>().unwrap(),
                    nums.trim()
                        .split_whitespace()
                        .map(|n| n.parse::<usize>().unwrap())
                        .collect(),
                );
            } else {
                unreachable!()
            }
        })
        .collect()
}

fn main() {
    println!("{}", part_one());
    println!("{}", part_two());
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &'static str = r"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
";

    #[test]
    fn example() {
        let s = parse(INPUT);
        assert_eq!(sum_sat_targets(s), 3749);
    }
    
    #[test]
    fn example2() {
        let s = parse(INPUT);
        assert_eq!(sum_sat_targets(s), 3749);
    }
}
