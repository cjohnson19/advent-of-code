use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Direction {
    Right,
    Left,
}

pub fn lcm(nums: &[usize]) -> usize {
    if nums.len() == 1 {
        return nums[0];
    }
    let a = nums[0];
    let b = lcm(&nums[1..]);
    a * b / gcd_of_two_numbers(a, b)
}

fn gcd_of_two_numbers(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    gcd_of_two_numbers(b, a % b)
}

impl From<char> for Direction {
    fn from(c: char) -> Self {
        match c {
            'R' => Direction::Right,
            'L' => Direction::Left,
            _ => panic!("Invalid direction"),
        }
    }
}

fn part_two(input: &str) -> String {
    let parsed: Vec<_> = input.split("\n\n").collect();
    let directions: Vec<_> = parsed[0].chars().map(Direction::from).collect();
    let rules: HashMap<_, _> = parsed[1]
        .lines()
        .map(|line| {
            let mut parts = line.split(" = ");
            let key = parts.next().unwrap().trim();
            let values: Vec<_> = parts.next().unwrap().trim().split(", ").collect();
            let left_value = values[0].replace('(', "");
            let left_value = left_value.trim();
            let right_value = values[1].replace(')', "");
            let right_value = right_value.trim();
            (
                key.to_string(),
                (left_value.to_string(), right_value.to_string()),
            )
        })
        .collect();
    let mut count: u64 = 0;
    let mut current_positions: Vec<String> = rules
        .clone()
        .keys()
        .filter(|x| x.ends_with("A"))
        .map(|x| x.clone())
        .collect();
    let mut end_position_indexes: Vec<Option<u64>> =
        (0..current_positions.len()).map(|_| None).collect();

    for dir in directions.iter().cycle() {
        current_positions.iter_mut().enumerate().for_each(|(i, x)| {
            if x.ends_with('Z') {
                if let None = end_position_indexes[i] {
                    end_position_indexes[i] = Some(count);
                }
            }
            let (left, right) = rules.get(x).unwrap();
            match dir {
                Direction::Right => *x = right.clone(),
                Direction::Left => *x = left.clone(),
            }
        });
        count += 1;

        if end_position_indexes.iter().all(|x| x.is_some()) {
            let end_position_indexes: Vec<_> = end_position_indexes
                .iter()
                .map(|x| x.unwrap() as usize)
                .collect();
            return lcm(&end_position_indexes).to_string();
        }
    }
    "0".to_string()
}

fn main() {
    let input = include_str!("./input.txt");
    println!("{}", part_two(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_two() {
        let input = r#"LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"#;
        assert_eq!(part_two(input), "6");
    }
}
