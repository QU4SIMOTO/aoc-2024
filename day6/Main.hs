{-# LANGUAGE TupleSections #-}
module Main where

import qualified Data.Array as A
import qualified Data.Set as S
import Text.Printf (printf)
import Data.Either (partitionEithers)
import Data.Bifunctor (bimap)
import Debug.Trace (trace)

type Part = String -> Int

data Dir
  = North
  | East
  | South
  | West
  deriving (Show, Eq, Ord, Enum)

data Cell = Empty | Obstacle deriving (Show, Eq)
data Guard = Guard Pos Dir deriving (Show, Eq, Ord)

type Pos = (Int, Int)
type Grid = A.Array Pos Cell

type Route = S.Set Guard

turn :: Guard -> Guard
turn (Guard pos d) = let d' = if d == West then North else succ d
  in Guard pos d'

parseDir :: Char -> Dir
parseDir '^' = North
parseDir '>' = East
parseDir 'v' = South
parseDir '<' = West
parseDir _ = error "Unknown direction"

parseCell :: Char -> Either Cell Dir
parseCell '#' = Left Obstacle
parseCell c 
  | c == '#' = Left Obstacle
  | c `elem` "^>v<" = Right (parseDir c)
  | otherwise = Left Empty

parse :: String -> (Grid, Guard)
parse input
  | null gs = error "No guard in data"
  | length gs > 1 = error "Multiple guards in data"
  | otherwise = (grid, guard) where
    rows = lines input
    bounds = ((0, 0), (length rows -1, length (head rows) - 1))
    (cs, gs) =
      partitionEithers [ bimap ((i,j),) ((i,j),) $ parseCell x
      | (xs, j) <- zip rows [0..], (x, i) <- zip xs [0..] ]
    guard@(Guard pos _) = uncurry Guard $ head gs
    grid = A.array bounds ((pos, Empty):cs)

move :: Guard -> Guard
move (Guard (x, y) dir) = case dir of 
  North -> Guard (x, y - 1) dir
  East  -> Guard (x + 1, y) dir
  South -> Guard (x, y + 1) dir
  West  -> Guard (x - 1, y) dir

isInBounds :: Pos -> Grid -> Bool
isInBounds (x, y) grid = x >= 0 && y >= 0 && x <= xMax && y <= yMax
  where (xMax, yMax) = snd $ A.bounds grid

route :: Grid -> Guard -> (Route, Bool)
route = route' S.empty False

route' :: Route -> Bool -> Grid -> Guard -> (Route, Bool)
route' r isCycle grid guard@(Guard pos dir)
  | isCycle = (r, isCycle)
  | not $ isInBounds pos' grid = (r, isCycle)
  | Empty <- grid A.! pos'
    = route' (S.insert guard' r) (S.member guard' r) grid guard'
  | otherwise = route' (S.insert guard'' r) (S.member guard'' r) grid guard''
    where (xMax, yMax) = snd $ A.bounds grid
          guard'@(Guard pos' _) = move guard
          guard'' = turn guard

part1 :: Part
part1 input = length $ S.insert g $ S.map (\(Guard pos _) -> pos) r
  where o@(_, Guard g _) = parse input
        r = fst $ uncurry route o

part2 :: Part
part2 input = length $ filter id $ map doesCreateCycle $ S.elems ps where
  (grid, guard@(Guard gPos _)) = parse input
  bounds = A.bounds grid
  ps = S.filter (/= gPos) $ S.map (\(Guard pos _) -> pos) $ fst $ route grid guard
  doesCreateCycle pos = snd $ route (grid A.// [(pos, Obstacle)]) guard

solve :: Part -> FilePath -> IO Int
solve part path = part <$> readFile path

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

