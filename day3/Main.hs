module Main where

import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Printf (printf)

data Operation
  = Do
  | Don't
  | Mul (Int, Int)
  deriving (Show)

type Program = [Operation]

type Part = String -> Int

parse :: String -> Program
parse input = map p (input =~ reg :: [[String]])
  where reg = "mul\\(([0-9]+),([0-9]+)\\)|don't\\(\\)|do\\(\\)"
        p ["do()", _, _] = Do
        p ["don't()", _, _] = Don't
        p [_, n, m] = Mul (read n, read m)

eval :: Program -> Int
eval = fst . foldl f (0, True)
  where f :: (Int, Bool) -> Operation -> (Int, Bool)
        f (n, _) Do = (n, True)
        f (n, _) Don't = (n, False)
        f (n, True) (Mul(x, y)) = (x * y + n, True)
        f (n, False) (Mul(_, _)) = (n, False)

part1 :: Part
part1 = eval . filter isMul . parse
  where isMul (Mul _) = True
        isMul _ = False

part2 :: Part
part2 = eval . parse

solve :: Part -> FilePath -> IO Int
solve part path = part <$> readFile path

main :: IO()
main = do
  solution1 <- solve part1 "./input.txt"
  solution2 <- solve part2 "./input.txt"
  printf "Part 1: %d\nPart 2: %d\n" solution1 solution2

