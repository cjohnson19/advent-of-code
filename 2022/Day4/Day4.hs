import Data.List

splitOnChar c s = [p1, p2]
  where
    p2 = drop 1 $ dropWhile (/= c) s
    p1 = takeWhile (/= c) s

stringToRange s = [st .. e]
  where
    st = toInteger $ read $ head l
    e = toInteger $ read $ last l
    l = splitOnChar '-' s

fullyContain l1 l2 = (==) n $ length $ intersect l1 l2
  where
    n = min (length l1) (length l2)

someContain l1 l2 = (> 0) $ length $ intersect l1 l2
  where
    n = min (length l1) (length l2)

part1 = length . filter (\x -> fullyContain (head x) (last x)) . map (map stringToRange . splitOnChar ',')

part2 = length . filter (\x -> someContain (head x) (last x)) . map (map stringToRange . splitOnChar ',')

main :: IO ()
main = do
  input <- readFile "4-1-sample.txt"
  print $ part1 $ lines input
  input <- readFile "4-1-sample.txt"
  print $ part2 $ lines input
  input <- readFile "4-1.txt"
  print $ part1 $ lines input
  input <- readFile "4-1.txt"
  print $ part2 $ lines input
