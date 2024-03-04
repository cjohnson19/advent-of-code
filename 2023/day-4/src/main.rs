fn get_input(file_name: &str) -> Vec<(Vec<usize>, Vec<usize>)> {
    let s = std::fs::read_to_string(file_name).unwrap();
    let lines = s.split("\n");

    lines
        .map(|line| {
            let parts: Vec<&str> = line.split(":").collect();
            let nums = parts[1];
            let parts: Vec<_> = nums.split("|").collect();
            let (winning_nums, hand_nums) = (parts[0].trim(), parts[1].trim());
            let winning_nums: Vec<_> = winning_nums
                .split_whitespace()
                .map(|s| s.trim().parse::<usize>().unwrap())
                .collect();
            let hand_nums: Vec<_> = hand_nums
                .split_whitespace()
                .map(|s| s.trim().parse::<usize>().unwrap())
                .collect();
            (winning_nums, hand_nums)
        })
        .collect()
}

fn part_one(name: &str) -> u32 {
    get_input(name)
        .iter()
        .map(|(w, h)| {
            let winning_numbers = w.iter().collect::<std::collections::HashSet<_>>();
            let winning_hand_count = h.iter().filter(|n| winning_numbers.contains(n)).count();
            if winning_hand_count == 0 {
                return 0;
            } else {
                return 2_u32.pow(winning_hand_count as u32 - 1);
            }
        })
        .sum()
}

fn part_two(name: &str) -> u32 {
    let input = get_input(name);
    let n = input.len();
    let mut scores = vec![1; n];
    for (i, (w, h)) in input.iter().enumerate() {
        let winning_numbers = w.iter().collect::<std::collections::HashSet<_>>();
        let winning_hand_count = h.iter().filter(|n| winning_numbers.contains(n)).count();
        for j in 1..=winning_hand_count {
            if (i + j) < n {
                scores[i + j] += scores[i];
            }
        }
    }
    return scores.iter().sum();
}

fn main() {
    println!("Part one sample: {}", part_one("sample.txt"));
    println!("Part one: {}", part_one("input.txt"));
    println!("Part two sample: {}", part_two("sample.txt"));
    println!("Part two: {}", part_two("input.txt"));
}
