struct Pos {
    x: i32,
    y: i32,
}

#[derive(Debug, PartialEq)]
enum Surface {
    VerticalPipe,
    HorizontalPipe,
    NorthAndEastPipe,
    NorthAndWestPipe,
    SouthAndWestPipe,
    SouthAndEastPipe,
    Ground,
    Start,
}

impl From<char> for Surface {
    fn from(c: char) -> Self {
        match c {
            '|' => Surface::VerticalPipe,
            '-' => Surface::HorizontalPipe,
            '7' => Surface::SouthAndWestPipe,
            'F' => Surface::SouthAndEastPipe,
            'L' => Surface::NorthAndEastPipe,
            'J' => Surface::NorthAndWestPipe,
            '.' => Surface::Ground,
            'S' => Surface::Start,
            _ => panic!("Invalid surface: {}", c),
        }
    }
}

impl Pos {
    fn succs(&self, current_surface: Surface) -> Vec<Pos> {
        let mut out = Vec::new();
        if current_surface == Surface::VerticalPipe {
            out.push(Pos {
                x: self.x,
                y: self.y + 1,
            });
            out.push(Pos {
                x: self.x,
                y: self.y - 1,
            });
        } else if current_surface == Surface::HorizontalPipe {
            out.push(Pos {
                x: self.x + 1,
                y: self.y,
            });
            out.push(Pos {
                x: self.x - 1,
                y: self.y,
            });
        } else if current_surface == Surface::NorthAndEastPipe {
            out.push(Pos {
                x: self.x,
                y: self.y + 1,
            });
            out.push(Pos {
                x: self.x + 1,
                y: self.y,
            });
        } else if current_surface == Surface::NorthAndWestPipe {
            out.push(Pos {
                x: self.x,
                y: self.y + 1,
            });
            out.push(Pos {
                x: self.x - 1,
                y: self.y,
            });
        } else if current_surface == Surface::SouthAndWestPipe {
            out.push(Pos {
                x: self.x,
                y: self.y - 1,
            });
            out.push(Pos {
                x: self.x - 1,
                y: self.y,
            });
        } else if current_surface == Surface::SouthAndEastPipe {
            out.push(Pos {
                x: self.x,
                y: self.y - 1,
            });
            out.push(Pos {
                x: self.x + 1,
                y: self.y,
            });
        }

        out
    }
}

fn main() {
    println!("Hello, world!")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pos() {
        let pos = Pos { x: 1, y: 2 };
        assert_eq!(pos.x, 1);
        assert_eq!(pos.y, 2);
    }
}
