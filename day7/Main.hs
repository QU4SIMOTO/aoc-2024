module Main where

import Data.List.Split (splitOn)
import Text.Printf (printf)

type Part = String -> Int

type Equation = (Int, [Int])
type Op = Int -> Int -> Int

parse :: String -> [Equation]
parse = map parseLine . lines where
  parseLine :: String -> Equation
  parseLine input = let [t, is] = splitOn ":" input
              in (read t, map read $ words is)

cat :: Int -> Int -> Int
cat x y = read $ show x ++ show y

hasSolution :: [Op] -> Equation -> Bool
hasSolution ops (t, xs) = hasSolution' ops t 0 xs where
  hasSolution' :: [Op] -> Int -> Int -> [Int] -> Bool
  hasSolution' _ t c [] = t == c
  hasSolution' ops t c (x:xs)
    = any (\o -> hasSolution' ops t (c `o` x) xs ) ops

part1 :: Part
part1 =  sum . map fst . filter (hasSolution [(+), (*)]) . parse

part2 :: Part
part2 =  sum . map fst . filter (hasSolution [(+), (*), cat]) . parse

solve :: Part -> FilePath -> IO Int
solve part path = part <$> readFile path

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

