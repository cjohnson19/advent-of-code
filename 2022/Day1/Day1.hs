import qualified Data.Text as T
import Data.Text.Read
import Data.List
import Data.Either

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : wordsWhen p s''
      where (w, s'') = break p s'

toElves = T.splitOn (T.pack "\n\n")

toElf = map T.words

sample = T.pack "1000\n\
\2000\n\
\3000\n\
\\n\
\4000\n\
\\n\
\5000\n\
\6000\n\
\\n\
\7000\n\
\8000\n\
\9000\n\
\\n\
\10000"

aux = map (((sum . map fst) . rights) . map decimal) . toElf . toElves

part1 = maximum . aux

part2 = sum . take 3 . reverse . sort . aux

main = do
  input <- readFile "1-1.txt"
  print $ part1 (T.pack input)
  print $ part2 (T.pack input)
