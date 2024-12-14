module Main where

import Text.Printf (printf)
import qualified Data.Map as M
import qualified Data.Set as S

type Part = String -> Int
type Vec2 = (Int, Int)
type Antenna = (Vec2, Char)

data AntennaMap = AntennaMap {
  items :: [Antenna],
  bounds :: (Int, Int)
} deriving (Show)

parse :: String -> AntennaMap
parse input = AntennaMap { items, bounds } where
  is = [ ((i, j), a)
          | (j, row) <- zip [0..] $ lines input
          , (i, a) <- zip [0..] row ]
  items = filter ((/='.') . snd) is
  bounds = fst $ last is

add :: Vec2 -> Vec2 -> Vec2
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sub :: Vec2 -> Vec2 -> Vec2
sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

mult :: Vec2 -> Int -> Vec2
mult (x, y) n = (x * n, y * n)

lte :: Vec2 -> Vec2 -> Bool
(x1, y1) `lte` (x2, y2) = x1 <= x2 && y1 <= y2

gte :: Vec2 -> Vec2 -> Bool
(x1, y1) `gte` (x2, y2) = x1 >= x2 && y1 >= y2

boundedBy :: (Vec2, Vec2) -> Vec2 -> Bool
boundedBy (lower, upper) v = v `gte` lower && v `lte` upper

aNodePairs :: Vec2 -> Vec2 -> [[Vec2]]
aNodePairs v1 v2 = [
    [sub v1 d', add v2 d'] | d' <- map (mult (sub v2 v1)) [0..]
  ]

aNodesWithTransform :: ([[Vec2]] -> [[Vec2]]) -> AntennaMap -> [Vec2]
aNodesWithTransform f (AntennaMap items uBound) = S.elems ns where
  getANodes :: Vec2 -> [Vec2] -> [Vec2]
  getANodes p = concat . concatMap (f . aNodePairs p)
  step (nSet, aMap) (p, a) = (nSet', aMap') where
    ps = M.findWithDefault [] a aMap
    ns = filter (boundedBy ((0,0), uBound)) $ getANodes p ps
    aMap' = M.insert a (p:ps) aMap
    nSet' = foldl (flip S.insert) nSet ns
  (ns, _ ) = foldl step (S.empty, M.empty) items

part1 :: Part
part1 = length . aNodesWithTransform (take 1 . tail) . parse

part2 :: Part
part2 input = length $ getAs m where
  m@(AntennaMap _ uBound) = parse input
  getAs = aNodesWithTransform (takeWhile $ any $ boundedBy ((0,0), uBound))

solve :: Part -> FilePath -> IO Int
solve part path = part <$> readFile path

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

