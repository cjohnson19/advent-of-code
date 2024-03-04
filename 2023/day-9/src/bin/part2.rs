fn differences(items: &Vec<i32>) -> Vec<i32> {
    items.windows(2).map(|w| w[1] - w[0]).collect()
}

fn part_two(input: &str) -> String {
    let histories: Vec<Vec<i32>> = input
        .lines()
        .map(|l| l.split_whitespace().map(|n| n.parse().unwrap()).collect())
        .collect();
    let mut next_nums: Vec<i32> = Vec::new();
    for history in histories {
        let mut inferences: Vec<Vec<i32>> = vec![history.clone()];
        loop {
            let latest_inf = differences(&inferences.last().unwrap());
            let all_zeroes = latest_inf.iter().all(|x| *x == 0);
            inferences.push(latest_inf);
            if all_zeroes {
                break;
            }
        }

        let mut last_val = 0;
        inferences.iter_mut().rev().skip(1).for_each(|v| {
            let cur_first = v.first().unwrap();
            last_val = cur_first - last_val;
            v.push(last_val)
        });

        next_nums.push(*inferences.first().unwrap().last().unwrap());
    }

    next_nums.iter().sum::<i32>().to_string()
}

fn main() {
    let input = include_str!("./input.txt");
    println!("{}", part_two(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_first() {
        let input = "0 3 6 9 12 15";
        assert_eq!(part_two(input), "-3");
    }

    #[test]
    fn test_second() {
        let input = r#"1 3 6 10 15 21"#;
        assert_eq!(part_two(input), "0");
    }

    #[test]
    fn test_rhird() {
        let input = r#"10 13 16 21 30 45"#;
        assert_eq!(part_two(input), "5");
    }

    #[test]
    fn test_large() {
        let input = r#"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"#;
        assert_eq!(part_two(input), "2");
    }
}
