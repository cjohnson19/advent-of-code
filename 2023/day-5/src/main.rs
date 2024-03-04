use indicatif::ParallelProgressIterator;
use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use std::{
    borrow::BorrowMut,
    collections::{HashMap, HashSet},
    fs,
};

fn part_one(seeds: &[usize], input: String) -> Result<String, String> {
    let maps = input
        .split("\n\n")
        .map(|map| {
            let mut lines = map.lines();
            let _ = lines.next().expect("Map has no label");
            lines
                .map(|line| {
                    let parts = line
                        .split_whitespace()
                        .map(|n| str::parse(n).unwrap())
                        .collect::<Vec<usize>>();
                    return (parts[0], parts[1], parts[2]);
                })
                .collect::<Vec<(usize, usize, usize)>>()
        })
        .collect::<Vec<_>>();

    let results = maps.iter().fold(seeds.to_vec(), |seeds, map| {
        seeds
            .iter()
            .map(|seed| {
                map.iter()
                    .find(|(_, src_start, length)| {
                        src_start <= seed && seed <= &(src_start + length)
                    })
                    .map(|(des_start, src_start, _)| des_start + seed - src_start)
                    .unwrap_or(*seed)
            })
            .collect()
    });
    let m = results.iter().min();
    Ok(m.unwrap().to_string())
}

fn in_ranges(ranges: &[(usize, usize)], q: usize) -> bool {
    ranges.iter().find(|(s, e)| *s <= q && q <= *e).is_some()
}

fn part_two(seeds: &[usize], input: String) -> Result<String, String> {
    let seed_ranges: Vec<(usize, usize)> =
        seeds.chunks(2).map(|v| (v[0], v[0]+v[1])).collect::<Vec<_>>();
    let mut maps = input
        .split("\n\n")
        .map(|map| {
            let mut lines = map.lines();
            let _ = lines.next().expect("Map has no lines");
            lines
                .map(|line| {
                    let parts = line
                        .split_whitespace()
                        .map(|n| str::parse(n).unwrap())
                        .collect::<Vec<usize>>();
                    return (parts[0], parts[1], parts[2]);
                })
                .collect::<Vec<(usize, usize, usize)>>()
        })
        .collect::<Vec<_>>();
    maps.reverse();

    let mut end = 0;
    loop {
        let start_pos = maps.iter().fold(end, |pos, maps| {
            maps.iter()
                .find(|(des_start, src_start, length)| {
                    *des_start <= pos && pos <= (des_start + length)
                })
                .map(|(des_start, src_start, l)| (pos - des_start) + src_start)
                .unwrap_or(pos)
        });
        if in_ranges(&seed_ranges, start_pos) {
            break;
        }
        end += 1;
    }
    return Ok(end.to_string());
}

fn main() {
    let input = fs::read_to_string("input.txt").unwrap();
    let seeds: Vec<usize> = vec![
        629551616, 310303897, 265998072, 58091853, 3217788227, 563748665, 2286940694, 820803307,
        1966060902, 108698829, 190045874, 3206262, 4045963015, 223661537, 1544688274, 293696584,
        1038807941, 31756878, 1224711373, 133647424,
    ];
    // let ans = part_one(&seeds, input.clone());
    let ans = part_two(&seeds, input.clone());
    println!("{}", ans.unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn part_one_test() {
        let input = r#"seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"#;
        let seeds: Vec<usize> = vec![79, 14, 55, 13];
        assert_eq!(part_one(&seeds, input.to_string()), Ok(35.to_string()));
    }

    #[test]
    fn part_two_test() {
        let input = r#"seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"#;
        let seeds: Vec<usize> = vec![79, 14, 55, 13];
        assert_eq!(part_two(&seeds, input.to_string()), Ok(46.to_string()));
    }
}
