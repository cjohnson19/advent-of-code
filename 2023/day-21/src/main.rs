use std::{
    collections::{HashSet, VecDeque},
    fmt, fs,
};

struct Grid {
    spaces: Vec<Vec<GridSpace>>,
}

impl Grid {
    fn new(spaces: Vec<Vec<GridSpace>>) -> Self {
        Grid { spaces }
    }

    fn get(&self, x: i128, y: i128) -> Option<&GridSpace> {
        let x = if x < 0 {
            self.spaces[0].len() as i128 - (x.abs() % self.spaces[0].len() as i128)
        } else {
            x % self.spaces[0].len() as i128
        };
        let y = if y < 0 {
            self.spaces.len() as i128 - (y.abs() % self.spaces.len() as i128)
        } else {
            y % self.spaces.len() as i128
        };
        self.spaces
            .get(y as usize)
            .and_then(|row| row.get(x as usize))
    }

    fn get_start_coords(&self) -> Option<(usize, usize)> {
        self.spaces.iter().enumerate().find_map(|(y, row)| {
            row.iter().enumerate().find_map(|(x, space)| match space {
                GridSpace::Start => Some((x, y)),
                _ => None,
            })
        })
    }

    fn set(&mut self, x: usize, y: usize, space: GridSpace) {
        self.spaces[y][x] = space;
    }
}

impl TryFrom<&str> for Grid {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let spaces = value
            .lines()
            .map(|l| l.chars().map(GridSpace::try_from).collect())
            .collect::<Result<_, _>>()?;
        Ok(Grid { spaces })
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.spaces {
            for space in row {
                write!(f, "{}", space)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

enum GridSpace {
    Empty,
    Blocked,
    Start,
}

impl TryFrom<char> for GridSpace {
    type Error = String;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '.' => Ok(GridSpace::Empty),
            '#' => Ok(GridSpace::Blocked),
            'S' => Ok(GridSpace::Start),
            _ => Err(format!("Invalid grid value: {}", value)),
        }
    }
}

impl fmt::Display for GridSpace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            GridSpace::Empty => write!(f, "."),
            GridSpace::Blocked => write!(f, "#"),
            GridSpace::Start => write!(f, "S"),
        }
    }
}

fn part_one(grid: &mut Grid, n: usize) -> usize {
    let mut queue: VecDeque<(i128, i128, usize)> = VecDeque::new();
    let mut seen: HashSet<(i128, i128)> = HashSet::new();
    let start_coords = grid.get_start_coords().expect("No start found");
    queue.push_back((start_coords.0 as i128, start_coords.1 as i128, 0));
    while !queue.is_empty() {
        queue.pop_front().map(|(x, y, steps)| {
            if let Some(GridSpace::Empty | GridSpace::Start) = grid.get(x, y) {
                if (n - steps) % 2 == 0 {
                    if seen.contains(&(x, y)) {
                        return;
                    }
                    seen.insert((x, y));
                }
                if steps == n {
                    seen.insert((x, y));
                    return;
                }
                queue.push_back((x + 1, y, steps + 1));
                queue.push_back((x - 1, y, steps + 1));
                queue.push_back((x, y + 1, steps + 1));
                queue.push_back((x, y - 1, steps + 1));
            }
        });
    }
    return seen.len();
}

fn main() -> Result<(), String> {
    let data = fs::read_to_string("input.txt").map_err(|e| e.to_string())?;
    let mut grid: Grid = data.as_str().try_into()?;
    println!("{}", grid);
    println!("{}", part_one(&mut grid, 26501365));
    Ok(())
}
