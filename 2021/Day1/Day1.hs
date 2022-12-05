increasedMeasurements :: Ord a => [a] -> [Bool]
increasedMeasurements l = zipWith (>) (tail l) l

threes l = zipWith3 (\x y z -> x + y + z) (tail $ tail l) (tail l) l

part1 = length . filter id . increasedMeasurements . map (toInteger . read)
part2 = length . filter id . increasedMeasurements . threes . map (toInteger . read)

main = do
  input <- readFile "1.txt"
  print $ part2 $ lines input
