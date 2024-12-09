module Main where

import Text.Printf (printf)
import Data.List.Split (splitOn)
import Data.List (any, sortBy)
import qualified Data.Map as M
import qualified Data.Set as S

type Part = String -> Int

type Rules = M.Map Int [Int]
type Update = [Int]
type Step = (Rules, Update, S.Set Int, Bool)

parse :: String -> (Rules, [Update])
parse input = (parseRules rs, parseUpdates us) where
  [rs, us] = splitOn "\n\n" input

parseRules :: String -> Rules
parseRules = M.fromListWith (++) . map parseRule . lines where
  parseRule r = (read l, [read u]) where
    [l, u] = splitOn "|" r

parseUpdates :: String -> [Update]
parseUpdates = map (map read . splitOn ",") . lines

isUpdateValid :: Rules -> Update -> Bool
isUpdateValid r u = isValid
  where (_, _, _, isValid) = isUpdateValid' (r, u, S.empty, True)

isUpdateValid' :: Step -> Step
isUpdateValid' i@(_, _, _, False) = i
isUpdateValid' i@(_, [], _, _) = i
isUpdateValid' i@(r, x:xs, s, _) = isUpdateValid' (r, xs, s', isValid) where
  ys = M.findWithDefault [] x r
  isValid = not $ any (`S.member` s) ys
  s' = S.insert x s

middle :: Update -> Int
middle xs = xs !! (length xs `div` 2)
  
part1 :: Part
part1 input = sum $ map middle $ filter (isUpdateValid rules) updates where
  (rules, updates) = parse input

fixUpdate :: Rules -> Update -> Update
fixUpdate r = sortBy (s r) where
  s r x y
    | M.member x r && y `elem` r M.! x = LT
    | M.member y r && x `elem` r M.! y = GT
    | otherwise = EQ

part2 :: Part
part2 input =
  let (rules, updates) = parse input
      invalidUpdates = filter (not . isUpdateValid rules) updates
  in sum . map (middle . fixUpdate rules) $ invalidUpdates

solve :: Part -> FilePath -> IO Int
solve part path = part <$> readFile path

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

