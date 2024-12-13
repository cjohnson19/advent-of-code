use std::{collections::HashSet, fs::read_to_string};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

struct Offset {
    x: i32,
    y: i32,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
struct Coordinate {
    x: usize,
    y: usize,
}

impl Direction {
    fn get_offset(self) -> Offset {
        match self {
            Direction::Up => Offset { y: -1, x: 0 },
            Direction::Down => Offset { y: 1, x: 0 },
            Direction::Left => Offset { y: 0, x: -1 },
            Direction::Right => Offset { y: 0, x: 1 },
        }
    }

    fn turn_right(self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
            Direction::Right => Direction::Down,
        }
    }
}

type Grid = Vec<Vec<char>>;

fn parse(s: &str) -> Grid {
    s.trim().lines().map(|l| l.chars().collect()).collect()
}

fn get_initial_position(g: &Grid) -> Option<Coordinate> {
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            if g[i][j] == '^' {
                return Some(Coordinate { y: i, x: j });
            }
        }
    }
    None
}

fn take_step(
    coord: &Coordinate,
    dir: &Direction,
    g: &Grid,
) -> Result<(Coordinate, Direction), String> {
    let mut new_dir = dir.clone();
    let mut new_coords;
    loop {
        let delta = new_dir.get_offset();
        if (coord.y as i32 + delta.y) < 0
            || (coord.x as i32 + delta.x) < 0
            || (coord.y as i32 + delta.y) as usize >= g.len()
            || (coord.x as i32 + delta.x) as usize >= g[0].len()
        {
            return Err("Out of bounds".to_string());
        }
        new_coords = Coordinate {
            x: (coord.x as i32 + delta.x) as usize,
            y: (coord.y as i32 + delta.y) as usize,
        };

        if g[new_coords.y][new_coords.x] == '#' {
            new_dir = new_dir.turn_right();
        } else {
            break;
        }
    }
    Ok((new_coords, new_dir))
}

fn traverse_grid(g: &Grid) -> usize {
    let mut seen_positions: HashSet<Coordinate> = HashSet::new();
    let mut cur_position = get_initial_position(&g).unwrap();
    let mut cur_dir = Direction::Up;
    loop {
        seen_positions.insert(cur_position);
        let res = take_step(&cur_position, &cur_dir, &g);
        match res {
            Ok((new_pos, new_dir)) => {
                cur_position = new_pos;
                cur_dir = new_dir;
            }
            Err(_) => {
                return seen_positions.len();
            }
        }
    }
}

fn count_loops(g: &Grid) -> usize {
    let mut count = 0;
    for i in 0..g.len() {
        for j in 0..g[i].len() {
            if g[i][j] == '#' || g[i][j] == '^' {
                continue;
            }
            let mut copy = g.clone();
            copy[i][j] = '#';
            if will_loop(&copy) {
                count += 1;
            }
        }
    }
    return count;
}

fn will_loop(g: &Grid) -> bool {
    let mut seen_positions: HashSet<(Coordinate, Direction)> = HashSet::new();
    let mut cur_position = get_initial_position(&g).unwrap();
    let mut cur_dir = Direction::Up;
    loop {
        if seen_positions.contains(&(cur_position, cur_dir)) {
            return true;
        }
        seen_positions.insert((cur_position, cur_dir));
        let res = take_step(&cur_position, &cur_dir, &g);
        match res {
            Ok((new_pos, new_dir)) => {
                cur_position = new_pos;
                cur_dir = new_dir;
            }
            Err(_) => {
                return false;
            }
        }
    }
}

fn part_one() -> String {
    let input = read_to_string("input.txt").unwrap();
    traverse_grid(&parse(&input)).to_string()
}

fn part_two() -> String {
    let input = read_to_string("input.txt").unwrap();
    count_loops(&parse(&input)).to_string()
}

fn main() {
    println!("{}", part_one());
    println!("{}", part_two());
}

#[cfg(test)]
mod test {
    use super::*;

    static INPUT: &'static str = r"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...";

    #[test]
    fn example() {
        let g = parse(&INPUT);
        assert_eq!(traverse_grid(&g), 41);
    }

    #[test]
    fn example2() {
        let g = parse(&INPUT);
        assert_eq!(count_loops(&g), 6);
    }
}
