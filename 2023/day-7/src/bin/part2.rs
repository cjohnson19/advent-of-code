use std::collections::HashMap;
use std::hash::Hash;
use std::str::FromStr;

#[derive(PartialEq, PartialOrd, Ord, Hash, Eq, Clone, Copy, Debug)]
enum Card {
    Joker,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
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
            "J" => Some(Joker),
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

#[derive(Eq, Ord, PartialEq, PartialOrd, Debug)]
enum Hand {
    High,
    Pair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

use itertools::Itertools;

use crate::Card::*;
use crate::Hand::*;

fn counter<I>(items: I) -> HashMap<I::Item, usize>
where
    I: IntoIterator,
    I::Item: Eq + Hash + Clone,
{
    let mut map = HashMap::new();
    for item in items {
        *map.entry(item.clone()).or_insert(0) += 1;
    }
    map
}

fn hand_from_cards(cards: &[Card]) -> Hand {
    let mut card_counter = counter(cards);
    let joker_count = card_counter.remove(&Joker).unwrap_or(0);
    let card_counter_copy = card_counter.clone();
    let most_common_entry = card_counter_copy.iter().max_by_key(|(_, c)| *c);
    let highest_occurrence = match most_common_entry {
        Some((c, n)) => {
            card_counter.remove(*c);
            *n
        }
        None => 0 as usize,
    };
    match highest_occurrence + joker_count {
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
            if card_counter.values().filter(|v| *v == &2).count() > 0 {
                TwoPair
            } else {
                Pair
            }
        }
        _ => High,
    }
}

fn part2(input: &str) -> String {
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

fn main() {
    let input = include_str!("./input.txt");
    println!("{}", part2(input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part_two() {
        let input = r#"32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"#;
        assert_eq!(part2(input), "5905".to_string());
    }

    #[test]
    fn joker_test_fh() {
        let cards = "JQQAA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FullHouse);
    }

    #[test]
    fn joker_test_four_kind() {
        let cards = "JQQQA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FourOfAKind);
    }

    #[test]
    fn joker_test_four_kind_joker() {
        let cards = "JJQQA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FourOfAKind);
    }

    #[test]
    fn test_four_kind() {
        let cards = "QQQQA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FourOfAKind);
    }

    #[test]
    fn test_full_house() {
        let cards = "QQQAA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FullHouse);
    }

    #[test]
    fn test_hand_from_cards_full_house() {
        let cards = "QQQAA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FullHouse);
    }

    #[test]
    fn test_hand_from_cards_four_of_a_kind() {
        let cards = "QQQQA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FourOfAKind);
    }

    #[test]
    fn test_hand_from_cards_joker_full_house() {
        let cards = "JQQAA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FullHouse);
    }

    #[test]
    fn test_hand_from_cards_joker_four_of_a_kind() {
        let cards = "JQQQA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FourOfAKind);
    }

    #[test]
    fn test_hand_from_cards_joker_four_of_a_kind_joker() {
        let cards = "JJQQA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, FourOfAKind);
    }

    #[test]
    fn test_part2_empty_input() {
        let input = "";
        assert_eq!(part2(input), "0".to_string());
    }

    #[test]
    fn test_hand_from_cards_pair() {
        let cards = "25QQA";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, Pair);
    }

    #[test]
    fn test_hand_from_cards_two_pair() {
        let cards = "Q54Q5";
        let cards_parsed: Vec<_> = cards
            .chars()
            .map(|c| c.to_string().parse::<Card>().unwrap())
            .collect();
        let hand = hand_from_cards(&cards_parsed);
        assert_eq!(hand, TwoPair);
    }
}
