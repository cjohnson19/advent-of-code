use std::fs::read_to_string;

use regex::Regex;

fn extract_multiplies(s: &str) -> i32 {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();

    re.captures_iter(s)
        .map(|c| c.extract())
        .map(|(_, [l, r])| l.parse::<i32>().unwrap() * r.parse::<i32>().unwrap())
        .sum()
}

fn extract_conditional_multiplies(s: &str) -> i32 {
    let content = s.split("don't").collect::<Vec<&str>>();
    let mut rest = content
        .iter()
        .skip(1)
        .map(|m| m.split("do").skip(1).collect::<Vec<&str>>().join(""))
        .collect::<Vec<String>>();
    rest.push(content[0].to_string());
    extract_multiplies(&rest.join(""))
}

fn part_one() -> String {
    let input = read_to_string("input.txt").unwrap();
    extract_multiplies(&input).to_string()
}

fn part_two() -> String {
    let input = read_to_string("input.txt").unwrap();
    extract_conditional_multiplies(&input).to_string()
}

fn main() {
    println!("Hello, world!");
    println!("{}", part_one());
    println!("{}", part_two());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn p1() {
        let s = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
        assert_eq!(extract_multiplies(&s), 161);
    }

    #[test]
    fn p2() {
        let s = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
        assert_eq!(extract_conditional_multiplies(&s), 48);
    }
}
