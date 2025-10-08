use std::str::FromStr;

use nalgebra::{Matrix2, Vector2}; // not strictly required to import; trait bound is needed

// const input: &str = include_str!("../input.txt");
const input: &str = include_str!("../actual_input.txt");

#[derive(Debug)]
struct Offset {
    x: usize,
    y: usize,
}

impl FromStr for Offset {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split(":").collect::<Vec<_>>();
        let offsets = parts[1].split(",").collect::<Vec<_>>();
        let x_offset = offsets[0].trim().split("+").collect::<Vec<_>>()[1]
            .parse::<usize>()
            .map_err(|_| "Error parsing X")?;
        let y_offset = offsets[1].trim().split("+").collect::<Vec<_>>()[1]
            .parse::<usize>()
            .map_err(|_| "Error parsing Y")?;
        Ok(Offset {
            x: x_offset,
            y: y_offset,
        })
    }
}

struct Coordinate {
    x: usize,
    y: usize,
}

impl FromStr for Coordinate {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts = s.split(":").collect::<Vec<_>>();
        let offsets = parts[1].split(",").collect::<Vec<_>>();
        let x_goal = offsets[0].trim().split("=").collect::<Vec<_>>()[1]
            .parse::<usize>()
            .map_err(|_| "Error parsing X")?;
        let y_goal = offsets[1].trim().split("=").collect::<Vec<_>>()[1]
            .parse::<usize>()
            .map_err(|_| "Error parsing Y")?;
        Ok(Self {
            x: x_goal,
            y: y_goal,
        })
    }
}

struct PuzzleInput {
    a: Offset,
    b: Offset,
    prize: Coordinate,
}

impl ToString for PuzzleInput {
    fn to_string(&self) -> String {
        let line1 = format!("Button A: X+{}, Y+{}", self.a.x, self.a.y);
        let line2 = format!("Button B: X+{}, Y+{}", self.b.x, self.b.y);
        let line3 = format!("Prize: X={}, Y={}", self.prize.x, self.prize.y);
        format!("{}\n{}\n{}", line1, line2, line3)
    }
}

fn get_inputs() -> Vec<PuzzleInput> {
    input
        .split("\n\n")
        .map(|puzzle| {
            if let [a_str, b_str, goal] = puzzle.lines().collect::<Vec<_>>().as_slice() {
                PuzzleInput {
                    a: a_str.parse().unwrap(),
                    b: b_str.parse().unwrap(),
                    prize: goal.parse().unwrap(),
                }
            } else {
                panic!("Ahh")
            }
        })
        .collect()
}

type Cost = usize;

impl PuzzleInput {
    fn solve(&self) -> Option<Cost> {
        let mut res = None;
        for a_press in 0..=100 {
            for b_press in 0..=100 {
                let cost = a_press * 3 + b_press;
                if res.is_some_and(|best| best <= cost) {
                    continue;
                }
                let x_pos = self.a.x * a_press + self.b.x * b_press;
                let y_pos = self.a.y * a_press + self.b.y * b_press;
                if x_pos == self.prize.x && y_pos == self.prize.y {
                    res = Some(cost);
                }
            }
        }
        res
    }

    fn solve2(&self) -> Option<Cost> {
        let x_goal = self.prize.x as f64 + 10_000_000_000_000.0;
        let y_goal = self.prize.y as f64 + 10_000_000_000_000.0;

        // M = [[a.x, b.x],
        //      [a.y, b.y]]
        let m = Matrix2::<f64>::new(
            self.a.x as f64,
            self.b.x as f64,
            self.a.y as f64,
            self.b.y as f64,
        );
        let b = Vector2::<f64>::new(x_goal, y_goal);

        m.lu()
            .solve(&b)
            .map(|x| {
                let a_press = x[0].round();
                let b_press = x[1].round();

                ((x[0] - a_press).abs() < 1e-3
                    && (x[1] - b_press).abs() < 1e-3
                    && a_press >= 0.0
                    && b_press >= 0.0)
                    .then_some((3.0 * a_press + b_press) as usize)
            })
            .flatten()
    }
}

fn main() {
    let inputs = get_inputs();
    // Debugging
    // for i in inputs {
    //     println!("{}", i.to_string());
    //     println!("Min cost: {:?}\n", i.solve());
    // }

    // Actual res
    let ans1 = inputs.iter().filter_map(|i| i.solve()).sum::<usize>();
    let ans2 = inputs.iter().filter_map(|i| i.solve2()).sum::<usize>();
    println!("Part 1: {}", ans1);
    println!("Part 2: {}", ans2);
}
