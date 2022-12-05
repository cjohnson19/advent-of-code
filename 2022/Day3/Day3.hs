import Data.Char ( isLower )
import Data.List ( nub )

priorityOf c
  | isLower c = fromEnum c - 96
  | otherwise = fromEnum c - 64 + 26

parts l = splitAt m l
  where m = length l `div` 2

shared (l1, l2) = filter (`elem` l2) l1

part1 = sum . map (sum . map priorityOf . nub . shared . parts)

part2 = sum . map (((sum . map priorityOf) . nub) . shared2) . groups

shared2 ls = filter (\x -> all (elem x) ls) l
  where l = head ls

-- Taken from data.list.split
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

groups = chunksOf 3

main :: IO ()
main = do
  input <- readFile "3-1.txt"
  print $ part1 (lines input)
  print $ part2 (lines input)
