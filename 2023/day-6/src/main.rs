fn part1(input: &str) -> String {
    let input_parsed = input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .skip(1)
                .map(|x| x.parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect::<Vec<Vec<i32>>>();
    let times = input_parsed[0].clone();
    let distances = input_parsed[1].clone();

    times
        .iter()
        .zip(distances)
        .map(|(t, d)| (0..*t).filter(|i| i * (t - i) > d).count() as i32)
        .product::<i32>()
        .to_string()
}

fn part2(input: &str) -> String {
    let input_parsed = input
        .lines()
        .map(|line| {
            line.split_whitespace()
                .skip(1)
                .collect::<Vec<_>>()
                .join("")
                .parse::<i64>()
                .unwrap()
        })
        .collect::<Vec<i64>>();
    let time = input_parsed[0].clone();
    let distance = input_parsed[1].clone();

    (0..time)
        .filter(|i| i * (time - i) > distance)
        .count()
        .to_string()
}

fn main() {
    let input = include_str!("./input.txt");
    println!("{}", part1(input));
    println!("{}", part2(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        let input = r#"Time:      7  15   30
Distance:  9  40  200"#;
        assert_eq!(part1(input), "288".to_string());
    }

    #[test]
    fn part_two() {
        let input = r#"Time:      7  15   30
Distance:  9  40  200"#;
        assert_eq!(part2(input), "71503".to_string());
    }
}

