{-# LANGUAGE TupleSections #-}

import Data.Array
import Data.List
import qualified Data.Text as T

getCratesFromRow :: String -> String
getCratesFromRow = reverse . fst . until (null . snd) step . ([],)
  where
    step (h, []) = (h, [])
    step (h, l) = step (l !! 1 : h, drop 4 l)

parseMove :: String -> (Int, Int, Int)
parseMove s = (src, dst, amt)
  where
    w = words s
    amt = read $ w !! 1
    src = read $ w !! 3
    dst = read $ w !! 5

adjustCrates :: Array Int String -> (Int, Int, Int) -> Array Int String
adjustCrates crates (src, dst, amt) =
  crates // [(src, drop amt cs), (dst, reverse (take amt cs) ++ crates ! dst)]
  where
    cs = crates ! src

adjustCrates2 :: Array Int String -> (Int, Int, Int) -> Array Int String
adjustCrates2 crates (src, dst, amt) =
  crates // [(src, drop amt cs), (dst, take amt cs ++ crates ! dst)]
  where
    cs = crates ! src

toArray :: [String] -> Array Int String
toArray s = listArray (1, length s) s

part1 :: String -> String
part1 s = map head $ elems $ foldl adjustCrates crates moves
  where
    moves = getMoves s
    crates = getCrates s

part2 :: String -> String
part2 s = map head $ elems $ foldl adjustCrates2 crates moves
  where
    moves = getMoves s
    crates = getCrates s

getCratesStrList :: String -> [String]
getCratesStrList = init . lines . T.unpack . head . T.splitOn (T.pack "\n\n") . T.pack

getMoves :: String -> [(Int, Int, Int)]
getMoves = map parseMove . lines . T.unpack . last . T.splitOn (T.pack "\n\n") . T.pack

getCrates :: String -> Array Int String
getCrates = toArray . map (T.unpack . T.strip . T.pack) . transpose . map getCratesFromRow . getCratesStrList

main :: IO ()
main = do
  input <- readFile "5.txt"
  print $ part1 input
  print $ part2 input
