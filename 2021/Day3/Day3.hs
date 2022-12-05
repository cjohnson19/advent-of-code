import Data.Char (digitToInt)
import Data.Function (on)
import Data.List

binaryStrToInt :: String -> Integer
binaryStrToInt = foldl' (\a x -> a * 2 + toInteger (digitToInt x)) 0

aux f l = head $ f (compare `on` length) l'
  where
    l' = group (sort l)

mostCommon = aux maximumBy

leastCommon = aux minimumBy

part1 = binaryStrToInt . map mostCommon . transpose . lines
part1' = binaryStrToInt . map leastCommon . transpose . lines

main = do
  input <- readFile "3.txt"
  print $ part1' input * part1 input
