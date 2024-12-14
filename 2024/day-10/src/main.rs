use std::{collections::HashSet, fs::read_to_string};

type Grid = Vec<Vec<usize>>;

fn main() {
    println!("{}", part_one());
    println!("{}", part_two());
}

fn neighbors(i: usize, j: usize, g: &Grid) -> Vec<(usize, usize)> {
    let mut res = Vec::new();
    let ii = i as i32;
    let ji = j as i32;
    inbounds(ii - 1, ji, g).map(|c| res.push(c));
    inbounds(ii + 1, ji, g).map(|c| res.push(c));
    inbounds(ii, ji - 1, g).map(|c| res.push(c));
    inbounds(ii, ji + 1, g).map(|c| res.push(c));
    return res;
}

fn inbounds(i: i32, j: i32, g: &Grid) -> Option<(usize, usize)> {
    if 0 <= i && 0 <= j && (i as usize) < g.len() && (j as usize) < g.len() {
        Some((i as usize, j as usize))
    } else {
        None
    }
}

fn trailhead_count_paths(i: usize, j: usize, n: usize, g: &Grid) -> Vec<Vec<(usize, usize)>> {
    if g[i][j] != n {
        return Vec::new();
    }
    if n == 9 {
        return vec![vec![(i, j)]];
    }
    neighbors(i, j, &g)
        .iter()
        .flat_map(|(new_i, new_j)| {
            let r = trailhead_count_paths(*new_i, *new_j, n + 1, &g);
            r.iter()
                .map(|p| {
                    let mut res = p.clone();
                    res.push((i, j));
                    res
                })
                .collect::<Vec<_>>()
        })
        .collect()
}

fn trailhead_count(i: usize, j: usize, n: usize, g: &Grid) -> Vec<(usize, usize)> {
    if g[i][j] != n {
        return Vec::new();
    }
    if n == 9 {
        return vec![(i, j)];
    }
    neighbors(i, j, &g)
        .iter()
        .flat_map(|(i, j)| trailhead_count(*i, *j, n + 1, &g))
        .collect()
}

fn get_trailhead_count(g: Grid) -> usize {
    let mut result = 0;
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            let res = trailhead_count(i, j, 0, &g);
            let s: HashSet<(usize, usize)> = HashSet::from_iter(res.iter().map(|l| l.to_owned()));
            result += s.len();
        }
    }

    result
}

fn get_trailhead_ratings(g: Grid) -> usize {
    let mut result = 0;
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            let res = trailhead_count_paths(i, j, 0, &g);
            let s: HashSet<Vec<(usize, usize)>> =
                HashSet::from_iter(res.iter().map(|l| l.to_owned()));
            result += s.len();
        }
    }

    result
}

fn parse(s: &str) -> Grid {
    s.trim()
        .lines()
        .map(|l| {
            l.trim()
                .chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect()
        })
        .collect()
}

fn part_one() -> String {
    let s = read_to_string("input.txt").unwrap();
    get_trailhead_count(parse(&s)).to_string()
}

fn part_two() -> String {
    let s = read_to_string("input.txt").unwrap();
    get_trailhead_ratings(parse(&s)).to_string()
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &'static str = r"89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732";

    #[test]
    fn example() {
        assert_eq!(get_trailhead_count(parse(INPUT)), 36)
    }

    #[test]
    fn example2() {
        assert_eq!(get_trailhead_ratings(parse(INPUT)), 81)
    }
}
