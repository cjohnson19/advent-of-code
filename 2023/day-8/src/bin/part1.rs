use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Direction {
    Right,
    Left,
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

fn part_one(input: &str) -> String {
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
    let mut count = 0;
    let mut current = "AAA";

    for dir in directions.iter().cycle() {
        let (left, right) = rules.get(current).unwrap();
        match dir {
            Direction::Right => {
                current = right;
            }
            Direction::Left => {
                current = left;
            }
        }
        count += 1;
        if current == "ZZZ" {
            return count.to_string();
        }
    }
    "0".to_string()
}

fn main() {
    let input = include_str!("./input.txt");
    println!("{}", part_one(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = r#"RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)"#;
        assert_eq!(part_one(input), "2");
    }

    #[test]
    fn test_part_one_repeat() {
        let input = r#"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"#;
        assert_eq!(part_one(input), "6");
    }
}
