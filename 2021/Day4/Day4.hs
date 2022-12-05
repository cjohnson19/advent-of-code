import Data.List
import qualified Data.Text as T

winningBoard b called = winningBoard' b || winningBoard' b'
  where
    winningBoard' l = any (all (`elem` called)) l
    b' = transpose b

boardScore :: [[Integer]] -> [Integer] -> Integer
boardScore b called = (*) d $ sum $ concatMap (filter (`notElem` called)) b
  where
    d = last called

winners boards called = filter (`winningBoard` called) boards

roundWinners boards called = map (`winners` called) boards

boardRounds :: [[[Integer]]] -> [Integer] -> [([[[Integer]]], [Integer])]
boardRounds boards nums = map (\x -> (boards, x)) l
  where l = inits nums

main = do
  input <- readFile "4-sample.txt"
  let i = T.splitOn (T.pack "\n\n") (T.pack input)
  let called = map (toInteger . read . T.unpack) (T.splitOn (T.pack ",") (head i))
  let boards = map (filter (not . null) . map (map (toInteger . read) . words . T.unpack) . T.splitOn (T.pack "\n")) (tail i)
  print $ (\(b, c) -> boardScore (head (winners b c)) c) $ head $ filter (\(b, c) -> not (null b)) $ map (\(b, c) -> (winners b c, c)) $ boardRounds boards called
