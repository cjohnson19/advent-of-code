toDelta s = aux w n
  where
    aux "forward" n = (0, n)
    aux "backward" n = (0, - n)
    aux "up" n = (- n, 0)
    aux "down" n = (n, 0)
    aux _ _ = undefined
    (w, n) = (\l -> (head l, toInteger (read (last l)))) $ words s

toDeltaAim s = aux w n
  where
    aux "forward" dx = (dx, 0)
    aux "up" da = (0, - da)
    aux "down" da = (0, da)
    aux _ _ = undefined
    (w, n) = (\l -> (head l, toInteger (read (last l)))) $ words s

updateDelta (dx, dy) (x, y) = (dx + x, dy + y)

updateDelta2 (x, y, a) (dx, da) = (dx + x, a' * dx + y, a')
  where a' = a + da

part1 :: [String] -> Integer
part1 = uncurry (*) . foldr (updateDelta . toDelta) (0, 0)

part2 :: [String] -> Integer
part2 = (\(x, y, _) -> x * y) . foldl updateDelta2 (0, 0, 0) . map toDeltaAim

main :: IO ()
main = do
  input <- readFile "2.txt"
  print $ part2 $ lines input
