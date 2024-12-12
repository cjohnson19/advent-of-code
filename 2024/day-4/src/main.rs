use std::{
    collections::{HashMap, HashSet},
    fs::read_to_string,
};

fn main() {
    println!("{}", part_one());
    println!("{}", part_two());
}

fn pos_diagonals(v: &Vec<Vec<char>>, y: usize, x: usize, r: usize) -> (String, String) {
    let r_i32 = r as i32;
    let mut down_left = String::new();
    let mut up_right = String::new();
    for dist in (-r_i32)..=r_i32 {
        if (dist + (y as i32) < 0) || (dist + (x as i32)) < 0 || ((y as i32) - dist) < 0 {
            continue;
        }
        let y_real = (dist + (y as i32)) as usize;
        let y_real2 = ((y as i32) - dist) as usize;
        let x_real = (dist + (x as i32)) as usize;
        if y_real >= v.len() || x_real >= v[y_real].len() || y_real2 >= v.len() {
            continue;
        }
        down_left.push(v[y_real][x_real]);
        up_right.push(v[y_real2][x_real]);
    }

    return (down_left, up_right);
}

fn transpose<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    let n = v[0].len();
    (0..n)
        .map(|i| v.iter().map(|x| x[i].clone()).collect::<Vec<T>>())
        .collect()
}

fn diagonals<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Copy,
{
    let rows = v.len();
    let cols = v[0].len();
    let mut diagonals: Vec<Vec<T>> = vec![Vec::new(); rows + cols];
    for i in 0..v.len() {
        for j in 0..v[i].len() {
            diagonals[i + j].push(v[i][j]);
        }
    }

    diagonals
}

fn reverse_rows<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    v.iter()
        .map(|inner| inner.iter().rev().map(|c| c.to_owned()).collect::<Vec<T>>())
        .collect()
}

fn invert_rows<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>>
where
    T: Clone,
{
    v.iter().rev().map(|v| v.to_owned()).collect()
}

fn cross_locations(search: &str, v: Vec<Vec<char>>) -> usize {
    let r = search.len() / 2;
    let search_rev = search.chars().rev().collect::<String>();
    let mut count = 0;
    for i in 0..v.len() {
        for j in 0..v[i].len() {
            let (dl, ur) = pos_diagonals(&v, i, j, r);
            if (dl == search || dl == search_rev) && (ur == search || ur == search_rev) {
                count += 1;
            }
        }
    }
    return count;
}

fn xmas_count(v: Vec<Vec<char>>) -> usize {
    v.iter()
        .flat_map(|inner| {
            inner
                .windows(4)
                .filter(|word| word.iter().collect::<String>() == "XMAS")
        })
        .count()
}

fn backwards_xmas(v: Vec<Vec<char>>) -> usize {
    xmas_count(reverse_rows(v))
}

fn downwards_xmas(v: Vec<Vec<char>>) -> usize {
    xmas_count(transpose(v))
}

fn upwards_xmas(v: Vec<Vec<char>>) -> usize {
    xmas_count(reverse_rows(transpose(v)))
}

fn down_right_diagonal(v: Vec<Vec<char>>) -> usize {
    xmas_count(diagonals(v))
}

fn up_left_diagonal(v: Vec<Vec<char>>) -> usize {
    down_left_diagonal(invert_rows(v))
}

fn up_right_diagonal(v: Vec<Vec<char>>) -> usize {
    down_right_diagonal(invert_rows(v))
}

fn down_left_diagonal(v: Vec<Vec<char>>) -> usize {
    xmas_count(diagonals(reverse_rows(v)))
}

fn count_xmas(s: &str) -> i32 {
    let m = to_matrix(s);
    (xmas_count(m.clone())
        + backwards_xmas(m.clone())
        + downwards_xmas(m.clone())
        + upwards_xmas(m.clone())
        + down_right_diagonal(m.clone())
        + up_left_diagonal(m.clone())
        + up_right_diagonal(m.clone())
        + down_left_diagonal(m.clone())) as i32
}

fn to_matrix(s: &str) -> Vec<Vec<char>> {
    s.trim().lines().map(|l| l.chars().collect()).collect()
}

fn part_one() -> String {
    let s = read_to_string("input.txt").unwrap();
    count_xmas(&s.trim()).to_string()
}

fn part_two() -> String {
    let s = read_to_string("input.txt").unwrap();
    cross_locations(&"MAS", to_matrix(&s)).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &'static str = r"MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX";

    #[test]
    fn p1_test() {
        let res = count_xmas(INPUT);
        assert_eq!(res, 18);
    }

    #[test]
    fn forward_test() {
        let res = xmas_count(to_matrix(INPUT));
        assert_eq!(res, 3);
    }

    #[test]
    fn backwards_test() {
        let res = backwards_xmas(to_matrix(INPUT));
        assert_eq!(res, 2);
    }

    #[test]
    fn down_test() {
        let res = downwards_xmas(to_matrix(INPUT));
        assert_eq!(res, 1);
    }

    #[test]
    fn up_test() {
        let res = upwards_xmas(to_matrix(INPUT));
        assert_eq!(res, 2);
    }

    #[test]
    fn dr_test() {
        let res = down_right_diagonal(to_matrix(INPUT));
        assert_eq!(res, 1);
    }

    #[test]
    fn dl_test() {
        let res = down_left_diagonal(to_matrix(INPUT));
        assert_eq!(res, 1);
    }

    #[test]
    fn ul_test() {
        let res = up_left_diagonal(to_matrix(INPUT));
        assert_eq!(res, 4);
    }

    #[test]
    fn ur_test() {
        let res = up_right_diagonal(to_matrix(INPUT));
        assert_eq!(res, 4);
    }

    #[test]
    fn simple_cross() {
        let input = r"AXAX
XBXX
CXCX
";
        let res = cross_locations("ABC", to_matrix(input));
        assert_eq!(res, 1);
    }

    #[test]
    fn intermediate_cross() {
        let input = r"XXXX
XXXX
AXAX
XBXX
CXCX
XXXX
XXXX";
        let res = cross_locations("ABC", to_matrix(input));
        assert_eq!(res, 1);
    }

    #[test]
    fn hard_cross() {
        let res = cross_locations("MAS", to_matrix(INPUT));
        assert_eq!(res, 9);
    }

    #[test]
    fn diagonals_test() {
        let input = r"AXC
XBX
AXC";
        dbg!(to_matrix(input));
        let res = pos_diagonals(&to_matrix(input), 1, 1, 1);
        assert_eq!(res, ("ABC".to_string(), "ABC".to_string()));
    }
}
