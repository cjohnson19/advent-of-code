use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
    os::unix::raw::gid_t,
};

type Grid = Vec<Vec<char>>;

fn parse(s: &str) -> Grid {
    s.trim()
        .lines()
        .map(|l| l.trim().chars().collect())
        .collect()
}

fn inbounds(g: &Grid, x: i32, y: i32) -> bool {
    0 <= x && x < g.len() as i32 && 0 <= y && y < g[x as usize].len() as i32
}

fn all_of_dist(
    g: &Grid,
    (x1, y1): (usize, usize),
    (x2, y2): (usize, usize),
) -> Vec<(usize, usize)> {
    let mut res = vec![(x1, y1), (x2, y2)];
    let x_offset = x2 as i32 - x1 as i32;
    let y_offset = y2 as i32 - y1 as i32;
    let mut new_x = x1 as i32;
    let mut new_y = y1 as i32;
    loop {
        new_x = new_x as i32 - x_offset as i32;
        new_y = new_y as i32 - y_offset as i32;
        if inbounds(g, new_x, new_y) {
            res.push((new_x as usize, new_y as usize));
        } else {
            break;
        }
    }
    return res;
}

fn inverse_dist(
    g: &Grid,
    (x1, y1): (usize, usize),
    (x2, y2): (usize, usize),
) -> Option<(usize, usize)> {
    let x_offset = x2 as i32 - x1 as i32;
    let y_offset = y2 as i32 - y1 as i32;
    let new_x = x1 as i32 - x_offset as i32;
    let new_y = y1 as i32 - y_offset as i32;
    if inbounds(g, new_x, new_y) {
        return Some((new_x as usize, new_y as usize));
    } else {
        None
    }
}

fn find_antinodes2(g: &Grid) -> usize {
    let mut frequencies: HashMap<char, Vec<(usize, usize)>> = HashMap::new();
    let mut antinodes: HashMap<char, HashSet<(usize, usize)>> = HashMap::new();
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            if g[i][j] == '.' {
                continue;
            }
            let freq = g[i][j];
            let found_antinodes = antinodes.entry(freq).or_insert_with(|| HashSet::new());
            let other_antennae = frequencies.entry(freq).or_insert_with(|| Vec::new());
            other_antennae
                .iter()
                .flat_map(|(x, y)| {
                    let rev = all_of_dist(&g, (*x, *y), (j, i));
                    let mut combined = all_of_dist(&g, (j, i), (*x, *y));
                    combined.extend(rev);
                    combined
                })
                .for_each(|coord| {
                    found_antinodes.insert(coord);
                });
            other_antennae.push((j, i));
        }
    }

    let mut antinode_locations: HashSet<(usize, usize)> = HashSet::new();

    antinodes.iter().for_each(|(_, locations)| {
        antinode_locations.extend(locations.iter());
    });

    antinode_locations.len()
}

fn find_antinodes(g: &Grid) -> usize {
    let mut frequencies: HashMap<char, Vec<(usize, usize)>> = HashMap::new();
    let mut antinodes: HashMap<char, HashSet<(usize, usize)>> = HashMap::new();
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            if g[i][j] == '.' {
                continue;
            }
            let freq = g[i][j];
            let found_antinodes = antinodes.entry(freq).or_insert_with(|| HashSet::new());
            let other_antennae = frequencies.entry(freq).or_insert_with(|| Vec::new());
            other_antennae
                .iter()
                .flat_map(|(x, y)| {
                    vec![
                        inverse_dist(&g, (j, i), (*x, *y)),
                        inverse_dist(&g, (*x, *y), (j, i)),
                    ]
                    .iter()
                    .filter_map(|p| *p)
                    .collect::<Vec<_>>()
                })
                .for_each(|coord| {
                    found_antinodes.insert(coord);
                });
            other_antennae.push((j, i));
        }
    }

    let mut antinode_locations: HashSet<(usize, usize)> = HashSet::new();

    antinodes.iter().for_each(|(_, locations)| {
        antinode_locations.extend(locations.iter());
    });

    antinode_locations.len()
}

fn part_one() -> String {
    let s = read_to_string("input.txt").unwrap();
    let g = parse(&s);
    find_antinodes(&g).to_string()
}

fn part_two() -> String {
    let s = read_to_string("input.txt").unwrap();
    let g = parse(&s);
    find_antinodes2(&g).to_string()
}

fn main() {
    println!("{}", part_one());
    println!("{}", part_two());
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &'static str = r"............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............";

    #[test]
    fn dist_tests() {
        let g = parse(INPUT);
        assert_eq!(inverse_dist(&g, (4, 3), (8, 4)), Some((0, 2)));
        // assert_eq!(find_antinodes(&parse(INPUT)), 14);
    }

    #[test]
    fn example() {
        assert_eq!(find_antinodes2(&parse(INPUT)), 34);
    }
}
