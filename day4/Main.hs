module Main where

import Data.List (transpose, isPrefixOf)
import Text.Printf (printf)
import qualified Data.Array as A

type Part = String -> Int

rows :: String -> [String]
rows = lines

cols :: String -> [String]
cols = transpose . lines

mapIndex :: (a -> Int -> b) -> [a] -> [b]
mapIndex f l = zipWith f l [0 .. length l - 1]

diags :: String -> [String]
diags i = diagl i ++ diagl' i ++ diagr i ++ diagr' i where
  diagl = transpose . mapIndex (flip drop) . lines
  diagr = transpose . mapIndex (flip drop) . map reverse . lines
  diagl' = tail . transpose . mapIndex (flip drop) . reverse . lines
  diagr' = tail . transpose . mapIndex (flip drop) . map reverse . reverse . lines

countOcurrences :: String -> String -> Int
countOcurrences word input = n where
  (n, _, _) = countWord'(0, word, input)
  countWord' (n, word, input)
    | word `isPrefixOf` input = countWord' (n + 1, word, drop (length word) input)
    | not $ null input = countWord' (n, word, drop 1 input)
    | otherwise = (n, word, input)

part1 :: Part
part1 = p "XMAS" where
  p word input = sum forwards + sum backwards where
    checkables = rows input ++ cols input ++ diags input
    forwards = map (countOcurrences word) checkables
    backwards = map (countOcurrences $ reverse word) checkables

type Pos = (Int, Int)
type Grid = A.Array Pos Char
type Crosses = (String, String)

createGrid :: String -> Grid
createGrid input = A.array bounds as where
  ls = lines input
  bounds = ((0, 0), (length ls - 1, length (head ls) - 1))
  as = [((i, j), x) | (xs, j) <- zip ls [0..], (x, i) <- zip xs [0..]]

innerIndices :: Grid -> [Pos]
innerIndices g = [(x, y) | x <- [1..x' - 1], y <- [1..y' - 1]] where
  (_, (x', y')) = A.bounds g

crosses :: Pos -> Grid -> Crosses
crosses p@(x, y) g = (topL:curr:[botR], topR:curr:[botL]) where
  curr = g A.! p
  topL = g A.! (x - 1, y - 1) 
  topR = g A.! (x + 1, y - 1) 
  botL = g A.! (x - 1, y + 1) 
  botR = g A.! (x + 1, y + 1) 

part2 :: Part
part2 input = length $ filter (\(s1, s2) -> check "MAS" s1 && check "MAS" s2) cs where
  g = createGrid input
  cs = map (`crosses` g) $ innerIndices g
  check word s = word == s || reverse word == s

solve :: Part -> FilePath -> IO Int
solve part path = part <$> readFile path

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

