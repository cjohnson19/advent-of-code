use std::{fs::read_to_string, slice::Iter};

fn main() {
    println!("Hello, world!");
    println!("{}", part_one());
    println!("{}", part_two());
    // println!(
    //     "{:?}",
    //     choose(&vec![1, 2, 3])
    //         .map(|v| v.collect::<Vec<_>>())
    //         .collect::<Vec<_>>()
    // );
}

fn part_one() -> String {
    let input_text = read_to_string("input.txt").unwrap();

    let reports: Vec<Vec<i32>> = input_text
        .trim()
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|n| n.parse::<i32>().unwrap())
                .collect()
        })
        .collect();

    reports
        .iter()
        .filter(|report| adjacent_level_check(report) && monotonic(report))
        .collect::<Vec<_>>()
        .len()
        .to_string()
}

fn choose<'a, T: Clone>(v: &'a Vec<T>) -> impl Iterator<Item = impl Iterator<Item = T> + 'a> + 'a {
    (0..v.len()).map(move |pivot| {
        v.iter()
            .take(pivot)
            .chain(v.iter().skip(pivot + 1))
            .cloned()
    })
}

fn part_two() -> String {
    let input_text = read_to_string("input.txt").unwrap();

    let reports: Vec<Vec<i32>> = input_text
        .trim()
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|n| n.parse::<i32>().unwrap())
                .collect()
        })
        .collect();

    reports
        .iter()
        .filter(|report| {
            return adjacent_level_check(&report) && monotonic(&report)
                || choose(report).any(|r| {
                    let v = r.collect::<Vec<_>>();
                    return adjacent_level_check(&v) && monotonic(&v);
                });
        })
        .collect::<Vec<_>>()
        .len()
        .to_string()
}

fn adjacent_level_check(v: &Vec<i32>) -> bool {
    v.windows(2).all(|v| {
        let diff = (v[1] - v[0]).abs();
        return 0 < diff && diff < 4;
    })
}

fn monotonic(v: &Vec<i32>) -> bool {
    v.windows(2).all(|v| v[1] - v[0] > 0) || v.windows(2).all(|v| v[1] - v[0] < 0)
}
