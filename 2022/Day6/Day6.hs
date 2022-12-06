import Data.List (nub)

listDistinct :: Eq a => [a] -> Bool
listDistinct l = length (nub l) == length l

getRunsOf :: Int -> [a] -> [[a]]
getRunsOf n xs
  | length xs < n = []
  | otherwise = take n xs : getRunsOf n (tail xs)

runsOfWithIndices :: Int -> [a] -> [(Int, [a])]
runsOfWithIndices n = zip [n ..] . getRunsOf n

firstUnique :: (Eq a) => [(Int, [a])] -> Int
firstUnique = fst . head . filter (\(i, l) -> listDistinct l)

part1 :: (Eq a) => [a] -> Int
part1 = firstUnique . runsOfWithIndices 4

part2 :: (Eq a) => [a] -> Int
part2 = firstUnique . runsOfWithIndices 14

main :: IO ()
main = do
  input <- readFile "6.txt"
  print $ part1 input
  print $ part2 input
