module Main where

import Data.List (any, splitAt)
import Text.Printf (printf)

type Report = [Level]
type Level = Int
type Part a = [Report] -> [a]

parse :: String -> [Report]
parse s = map parse_line $ lines s
  where parse_line l =  map read $ words l

diffs :: Report -> [Int]
diffs r = zipWith (-) r $ tail r

isSafe :: Report -> Bool 
isSafe r = let ds = diffs r
               ds' = map abs ds
               min = minimum ds
               max = maximum ds
               rule1 = compare 0 min == compare 0 max
               rule2 = maximum ds' < 4 && minimum ds' > 0
           in rule1 && rule2

removeNth :: Int -> [a] -> [a]
removeNth 0 xs = tail xs
removeNth n xs = head xs: removeNth (n-1) (tail xs)

possibleReports :: Report -> [Report]
possibleReports r = [removeNth n r | n <- [0..length r - 1]]

part1 :: Part Report
part1 = filter isSafe

part2 :: Part [Report]
part2 = filter (any isSafe) . map possibleReports

solve :: Part a -> FilePath -> IO Int
solve part input = length . part . parse <$> readFile input

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

