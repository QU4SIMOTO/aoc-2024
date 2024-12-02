module Main where

import Data.List (transpose, sort)
import Text.Printf (printf)

parse :: String -> [[Int]]
parse input = transpose . map parse_line $ lines input
  where parse_line l = map read $ words l

type Part = [[Int]] -> Int

part1 :: Part
part1 [xs, ys] = sum $ zipWith diff s1 s2
  where
    diff n m = abs $ n - m
    (s1, s2) = (sort xs, sort ys)

part2 :: Part
part2 [xs, ys] = sum [(*x) $ length $ filter (== x) ys | x <- xs]

solve :: Part -> FilePath -> IO Int
solve part input = part . parse <$> readFile input

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

