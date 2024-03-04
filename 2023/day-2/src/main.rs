use std::collections::HashMap;

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
enum Color {
    Red,
    Blue,
    Green,
}

impl TryFrom<&str> for Color {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value.trim() {
            "red" => Ok(Color::Red),
            "blue" => Ok(Color::Blue),
            "green" => Ok(Color::Green),
            _ => Err("Could not parse color"),
        }
    }
}

struct Reveal {
    pub count: usize,
    pub color: Color,
}

impl TryFrom<&str> for Reveal {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let parts: Vec<&str> = value.trim().split(" ").collect();

        if let [count, color] = parts[..] {
            let count = count
                .parse::<usize>()
                .or_else(|_| Err("Could not parse count"))?;
            let color = Color::try_from(color)?;
            Ok(Reveal { count, color })
        } else {
            Err("Could not parse reveal")
        }
    }
}

struct Turn {
    pub reveals: Vec<Reveal>,
}

impl TryFrom<&str> for Turn {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let reveals = value
            .trim()
            .split(",")
            .map(Reveal::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Turn { reveals })
    }
}

impl Turn {
    pub fn get_color_counts(&self) -> HashMap<Color, usize> {
        let mut map = HashMap::new();
        for reveal in &self.reveals {
            *map.entry(reveal.color).or_insert(0) += reveal.count;
        }
        map
    }
}

struct Game {
    pub id: usize,
    pub turns: Vec<Turn>,
}

impl TryFrom<&str> for Game {
    type Error = &'static str;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let parts: Vec<_> = value.split(":").take(2).collect();

        if let [game, turns] = &parts[..] {
            let id = game
                .split(" ")
                .last()
                .ok_or_else(|| "No id provided for game")?
                .parse::<usize>()
                .or_else(|_| Err("Could not parse id"))?;
            let turns: Vec<Turn> = turns
                .trim()
                .split(";")
                .map(Turn::try_from)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(Game { id, turns })
        } else {
            Err("Could not parse game")
        }
    }
}

impl Game {
    pub fn get_color_max_counts(&self) -> HashMap<Color, usize> {
        let mut map = HashMap::new();
        for turn in &self.turns {
            for (color, count) in turn.get_color_counts() {
                let val = map.get(&color).unwrap_or(&0).clone();
                map.insert(color, val.max(count));
            }
        }
        map
    }

    fn power(&self) -> usize {
        let mut power = 1;
        for (_, max_needed) in &self.get_color_max_counts() {
            power *= max_needed;
        }
        return power;
    }

    fn from_file(path: &str) -> Result<Vec<Game>, &'static str> {
        let contents = std::fs::read_to_string(path).or_else(|_| Err("Could not read file"))?;
        let games = contents
            .split("\n")
            .map(Game::try_from)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(games)
    }

    fn is_possible(&self, allowed: &HashMap<Color, usize>) -> bool {
        let max_counts = &self.get_color_max_counts();
        for (color, count) in allowed {
            if max_counts.get(&color).unwrap_or(&0) > &count {
                return false;
            }
        }
        return true;
    }
}

fn main() {
    let games = Game::from_file("input.txt").unwrap();

    let allowed = HashMap::from([(Color::Red, 12), (Color::Green, 13), (Color::Blue, 14)]);

    let good: usize = games
        .iter()
        .filter(|g| g.is_possible(&allowed))
        .map(|g| g.id)
        .sum();
    let powers: usize = games.iter().map(|g| g.power()).sum();
    println!("{:?}", good);
    println!("{:?}", powers);
}
