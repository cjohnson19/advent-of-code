use std::{collections::HashSet, fs::read_to_string, hash::Hash};

type Grid = Vec<Vec<char>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Dir {
    Up,
    Down,
    Right,
    Left,
}

fn main() {
    println!("Hello, world!");
    println!("{}", part_one());
    println!("{}", part_two());
}

fn part_one() -> String {
    let s = read_to_string("input.txt").unwrap();
    calculate_cost(&parse(&s)).to_string()
}

fn part_two() -> String {
    let s = read_to_string("input.txt").unwrap();
    calculate_cost_sides(&parse(&s)).to_string()
}

fn parse(s: &str) -> Grid {
    s.trim()
        .lines()
        .map(|l| l.trim().chars().collect())
        .collect()
}

fn neighbors_with_dir(i: usize, j: usize, g: &Grid) -> Vec<(Option<(usize, usize)>, Dir)> {
    let ii = i as i32;
    let ji = j as i32;
    return vec![
        (inbounds(ii - 1, ji, g), Dir::Up),
        (inbounds(ii + 1, ji, g), Dir::Down),
        (inbounds(ii, ji - 1, g), Dir::Left),
        (inbounds(ii, ji + 1, g), Dir::Right),
    ];
}

fn neighbors(i: usize, j: usize, g: &Grid) -> Vec<Option<(usize, usize)>> {
    let ii = i as i32;
    let ji = j as i32;
    return vec![
        inbounds(ii - 1, ji, g),
        inbounds(ii + 1, ji, g),
        inbounds(ii, ji - 1, g),
        inbounds(ii, ji + 1, g),
    ];
}

fn sides(
    i: usize,
    j: usize,
    c: char,
    g: &Grid,
    seen: &mut HashSet<(usize, usize)>,
) -> HashSet<(usize, usize, Dir)> {
    if seen.contains(&(i, j)) || g[i][j] != c {
        return HashSet::new();
    }
    seen.insert((i, j));
    let c = g[i][j];
    let mut r = HashSet::new();
    for (n, d) in neighbors_with_dir(i, j, g).iter() {
        match n {
            Some((ii, jj)) => {
                if g[*ii][*jj] != c {
                    // Internal boundary, add as a side
                    r.insert((i, j, *d));
                } else {
                    // Internal node, let the recursive call return the actual sides
                    r.extend(sides(*ii, *jj, c, g, seen).iter());
                }
            }
            None => {
                // Boundary, add as a side
                r.insert((i, j, *d));
            }
        }
    }

    r
}

fn side_count(mut sides: HashSet<(usize, usize, Dir)>) -> usize {
    for (i, j, d) in sides.clone().iter() {
        match d {
            // Only take the rightmost up / down direction
            Dir::Up => {
                let mut cur: i32 = (*j as i32) - 1;
                while cur >= 0 && sides.remove(&(*i, cur as usize, Dir::Up)) {
                    cur -= 1;
                }
            }
            Dir::Down => {
                let mut cur: i32 = (*j as i32) - 1;
                while cur >= 0 && sides.remove(&(*i, cur as usize, Dir::Down)) {
                    cur -= 1;
                }
            }
            // Take only the bottommost right / left side
            Dir::Right => {
                let mut cur: i32 = (*i as i32) - 1;
                while cur >= 0 && sides.remove(&(cur as usize, *j, Dir::Right)) {
                    cur -= 1;
                }
            }
            Dir::Left => {
                let mut cur: i32 = (*i as i32) - 1;
                while cur >= 0 && sides.remove(&(cur as usize, *j, Dir::Left)) {
                    cur -= 1;
                }
            }
        }
    }

    sides.len()
}

fn perimeter(i: usize, j: usize, c: char, g: &Grid, seen: &mut HashSet<(usize, usize)>) -> usize {
    if seen.contains(&(i, j)) || g[i][j] != c {
        return 0;
    }
    seen.insert((i, j));
    let c = g[i][j];
    let mut r = 0;
    for n in neighbors(i, j, g).iter() {
        match n {
            Some((ii, jj)) => {
                if g[*ii][*jj] != c {
                    r += 1;
                } else {
                    r += perimeter(*ii, *jj, c, g, seen);
                }
            }
            None => r += 1,
        }
    }

    r
}

fn area(i: usize, j: usize, c: char, g: &Grid, seen: &mut HashSet<(usize, usize)>) -> usize {
    if seen.contains(&(i, j)) || g[i][j] != c {
        return 0;
    }
    let mut r = 1;
    seen.insert((i, j));
    for n in neighbors(i, j, g).iter() {
        match n {
            Some((ii, jj)) => {
                r += area(*ii, *jj, c, g, seen);
            }
            None => (),
        }
    }

    r
}

fn calculate_cost_sides(g: &Grid) -> usize {
    let mut res = 0;
    let mut seen_perimeters: HashSet<(usize, usize)> = HashSet::new();
    let mut seen_areas: HashSet<(usize, usize)> = HashSet::new();
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            if !seen_perimeters.contains(&(i, j)) {
                let c = g[i][j];
                let p = side_count(sides(i, j, c, g, &mut seen_perimeters));
                let a = area(i, j, c, g, &mut seen_areas);
                // println!("Region of {} has {} sides and area of {}", g[i][j], p, a);
                res += p * a;
            }
        }
    }

    res
}

fn calculate_cost(g: &Grid) -> usize {
    let mut res = 0;
    let mut seen_perimeters: HashSet<(usize, usize)> = HashSet::new();
    let mut seen_areas: HashSet<(usize, usize)> = HashSet::new();
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            if !seen_perimeters.contains(&(i, j)) {
                let c = g[i][j];
                let p = perimeter(i, j, c, g, &mut seen_perimeters);
                let a = area(i, j, c, g, &mut seen_areas);
                res += p * a;
            }
        }
    }

    res
}

fn inbounds(i: i32, j: i32, g: &Grid) -> Option<(usize, usize)> {
    if 0 <= i && 0 <= j && (i as usize) < g.len() && (j as usize) < g[i as usize].len() {
        Some((i as usize, j as usize))
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &'static str = r"AAAA
BBCD
BBCC
EEEC";

    static INPUT2: &'static str = r"RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE";

    #[test]
    fn example() {
        assert_eq!(calculate_cost(&parse(INPUT)), 140);
    }
    #[test]
    fn example2() {
        assert_eq!(calculate_cost(&parse(INPUT2)), 1930);
    }

    #[test]
    fn example3() {
        assert_eq!(calculate_cost_sides(&parse(INPUT)), 80);
    }
    #[test]
    fn example4() {
        assert_eq!(calculate_cost_sides(&parse(INPUT2)), 1206);
    }
}
