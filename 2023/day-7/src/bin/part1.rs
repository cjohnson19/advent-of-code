use std::collections::HashMap;
use std::hash::Hash;
use std::str::FromStr;

use itertools::Itertools;

#[derive(PartialEq, PartialOrd, Ord, Hash, Eq, Clone, Copy, Debug)]
enum Card {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl FromStr for Card {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let try_match = match s {
            "A" => Some(Ace),
            "K" => Some(King),
            "Q" => Some(Queen),
            "J" => Some(Jack),
            "T" => Some(Ten),
            "9" => Some(Nine),
            "8" => Some(Eight),
            "7" => Some(Seven),
            "6" => Some(Six),
            "5" => Some(Five),
            "4" => Some(Four),
            "3" => Some(Three),
            "2" => Some(Two),
            _ => None,
        };
        try_match.ok_or("No match".to_string())
    }
}

#[derive(Eq, Ord, PartialEq, PartialOrd)]
enum Hand {
    High,
    Pair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

use crate::Card::*;
use crate::Hand::*;

fn counter<T: Eq + Hash + Clone>(elems: &[T]) -> HashMap<T, usize> {
    let mut map = HashMap::new();
    for item in elems {
        *map.entry(item.clone()).or_insert(0) += 1;
    }
    map
}

fn hand_from_cards(cards: &[Card]) -> Hand {
    let card_counter = counter(cards);
    let highest_occurrence = card_counter.values().max().unwrap();
    match highest_occurrence {
        5 => FiveOfAKind,
        4 => FourOfAKind,
        3 => {
            if card_counter.values().contains(&2) {
                FullHouse
            } else {
                ThreeOfAKind
            }
        }
        2 => {
            if card_counter.values().filter(|v| *v == &2).count() > 1 {
                TwoPair
            } else {
                Pair
            }
        }
        _ => High,
    }
}

fn part1(input: &str) -> String {
    input
        .lines()
        .map(|l| {
            let line: Vec<&str> = l.split_whitespace().collect();
            (
                line[0]
                    .chars()
                    .map(|s| s.to_string().parse::<Card>().unwrap())
                    .collect::<Vec<_>>(),
                line[1].parse::<u64>().unwrap(),
            )
        })
        .sorted_by_key(|i| (hand_from_cards(&i.0), i.0.clone()))
        .enumerate()
        .map(|(i, (_, c))| ((i as u64) + 1) * c)
        .sum::<u64>()
        .to_string()
}

fn part2(input: &str) -> String {
    todo!()
}

fn main() {
    let input = include_str!("./input.txt");
    println!("{}", part1(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_one() {
        let input = r#"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"#;
        assert_eq!(part1(input), "6440".to_string());
    }
}
