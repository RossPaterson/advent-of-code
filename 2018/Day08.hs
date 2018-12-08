module Main where

import Utilities

-- Input processing

type Input = Tree

data Tree = Tree [Tree] [Int]
  deriving Show

parse :: String -> Input
parse = fst . takeTree . map read . words

-- Scan one tree from the list
-- First two numbers are number of subtress and number of data values
takeTree :: [Int] -> (Tree, [Int])
takeTree (nc:nd:xs) = (Tree children (take nd rest), drop nd rest)
  where
    (children, rest) = takeTrees nc xs

-- Scan n trees from the list
takeTrees :: Int -> [Int] -> ([Tree], [Int])
takeTrees n xs
  | n == 0 = ([], xs)
  | otherwise = (t:ts, rest)
  where
    (t, xs') = takeTree xs
    (ts, rest) = takeTrees (n-1) xs'

-- Part One

solve1 :: Input -> Int
solve1 = sumTree

sumTree :: Tree -> Int
sumTree (Tree ts ns) = sum (map sumTree ts) + sum ns

testInput :: String
testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

tests1 :: [(String, Int)]
tests1 = [(testInput, 138)]

-- Part Two

solve2 :: Input -> Int
solve2 = value

value :: Tree -> Int
value (Tree [] ns) = sum ns
-- numbers are indices to the child trees, counting from 1
value (Tree ts is) = sum [if i <= n then vs !! (i-1) else 0 | i <- is]
  where
    n = length ts
    vs = map value ts

tests2 :: [(String, Int)]
tests2 = [(testInput, 66)]

main :: IO ()
main = do
    s <- readFile "input08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
