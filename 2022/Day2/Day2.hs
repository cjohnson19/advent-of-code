shapePoints s = case s of
  'X' -> 1
  'Y' -> 2
  'Z' -> 3
  _ -> 0

shapePoints2 s1 s2 = case (s1, s2) of
  ('A', 'Y') -> shapePoints 'X'
  ('B', 'Y') -> shapePoints 'Y'
  ('C', 'Y') -> shapePoints 'Z'
  ('A', 'X') -> shapePoints 'Z'
  ('B', 'X') -> shapePoints 'X'
  ('C', 'X') -> shapePoints 'Y'
  ('A', 'Z') -> shapePoints 'Y'
  ('B', 'Z') -> shapePoints 'Z'
  ('C', 'Z') -> shapePoints 'X'

-- s1 is opponent s2 is us
winLossPoints s1 s2 =
  case (s1, s2) of
    ('A', 'Y') -> 6
    ('B', 'Z') -> 6
    ('C', 'X') -> 6
    ('A', 'X') -> 3
    ('B', 'Y') -> 3
    ('C', 'Z') -> 3
    ('A', 'Z') -> 0
    ('B', 'X') -> 0
    ('C', 'Y') -> 0
    _ -> 0

winLossPointsDet 'X' = 0
winLossPointsDet 'Y' = 3
winLossPointsDet 'Z' = 6
winLossPointsDet _ = 0

pointsForRound (s1, s2) =
  winLossPoints s1 s2 + shapePoints s2

pointsForRound2 (s1, s2) =
  winLossPointsDet s2 + shapePoints2 s1 s2

sample =
  "A Y\n\
  \B X\n\
  \C Z"

part1 = sum . map (pointsForRound . (\x -> (head x, last x))) . lines

part2 = sum . map (pointsForRound2 . (\x -> (head x, last x))) . lines

main = do
  input <- readFile "2-1.txt"
  print $ part1 input
  print $ part2 input
