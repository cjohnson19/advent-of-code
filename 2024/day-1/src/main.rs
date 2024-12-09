use std::{collections::HashMap, fs::read_to_string};

fn main() {
    println!("{}", part_one().unwrap());
    println!("{}", part_two().unwrap());
}

fn part_one() -> Result<String, String> {
    let res = read_to_string("input.txt").unwrap();
    let mut lhs: Vec<i32> = Vec::new();
    let mut rhs: Vec<i32> = Vec::new();
    for line in res.trim().lines() {
        let nums = line.split_whitespace().collect::<Vec<_>>();
        lhs.push(nums[0].parse::<i32>().unwrap());
        rhs.push(nums[1].parse::<i32>().unwrap());
    }

    lhs.sort();
    rhs.sort();

    let res: i32 = lhs.iter().zip(rhs.iter()).map(|(l, r)| (r - l).abs()).sum();

    Ok(res.to_string())
}

fn part_two() -> Result<String, String> {
    let res = read_to_string("input.txt").unwrap();
    let mut lhs: Vec<i32> = Vec::new();
    let mut rhs: HashMap<i32, i32> = HashMap::new();
    for line in res.trim().lines() {
        let nums = line.split_whitespace().collect::<Vec<_>>();
        lhs.push(nums[0].parse::<i32>().unwrap());
        let right_num = nums[1].parse::<i32>().unwrap();
        rhs.insert(right_num, *rhs.get(&right_num).unwrap_or(&0) + 1);
    }

    let res: i32 = lhs.iter().map(|n| rhs.get(&n).unwrap_or(&0) * n).sum();

    Ok(res.to_string())
}
