import Data.List
import Data.Char

isVisible :: (Int, Int) -> [[Int]] -> Bool
isVisible (row, col) trees =
  isBorder || allSmaller treeRowBefore || allSmaller treeRowAfter || allSmaller treeColBefore || allSmaller treeColAfter
  where
    isBorder = row == 0 || col == 0 || row == (length trees - 1) || col == (length (trees !! row) - 1)
    allSmaller = all (< treeHeight)
    treeHeight = trees !! row !! col
    treeRow = trees !! row
    treeRowBefore = take col treeRow
    treeRowAfter = drop (col + 1) treeRow
    treeCol = transpose trees !! col
    treeColBefore = take row treeCol
    treeColAfter = drop (row + 1) treeCol

countVisible :: (Int, Int) -> [[Int]] -> Int
countVisible (row, col) trees =
  countSmaller (reverse treeRowBefore) * countSmaller treeRowAfter * countSmaller (reverse treeColBefore) * countSmaller treeColAfter
  where
    countSmaller = takeVisible
    treeHeight = trees !! row !! col
    treeRow = trees !! row
    treeRowBefore = take col treeRow
    treeRowAfter = drop (col + 1) treeRow
    treeCol = transpose trees !! col
    treeColBefore = take row treeCol
    treeColAfter = drop (row + 1) treeCol
    takeVisible [] = 0
    takeVisible (x:xs)
      | x < treeHeight = 1 + takeVisible xs
      | otherwise = 1

listToIndices l = [(r, c) | r <- [0 .. rows], c <- [0 .. cols]]
  where
    rows = length l - 1
    cols = length (head l) - 1

part1 l = length $ filter id $ map (`isVisible` l) (listToIndices l)
part2 l = maximum $ map (`countVisible` l) (listToIndices l)

toGrid :: String -> [[Int]]
toGrid = map (map digitToInt) . lines

main :: IO ()
main = do
  input <- readFile "8.txt"
  print $ part2 $ toGrid input
