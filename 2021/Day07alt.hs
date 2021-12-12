module Main where

import Utilities
import Data.List

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = readNumbers

-- Part One

distance :: Int -> Int -> Int
distance x1 x2 = abs (x1 - x2)

-- cost of moving all xs to v, given the cost on moving each)
total_cost :: (Int -> Int -> Int) -> [Int] -> Int -> Int
total_cost cost xs v = sum (map (cost v) xs)

-- approximate median
median :: Ord a => [a] -> a
median xs = sort xs!!(length xs `div` 2)

solve1 :: Input -> Int
solve1 xs = total_cost distance xs (median xs)

testInput :: String
testInput = "16,1,2,0,4,2,7,1,2,14"

tests1 :: [(String, Int)]
tests1 = [(testInput, 37)]

-- Part Two

-- assuming non-negative n, same as sum [1..n]
sum_triangle :: Int -> Int
sum_triangle n = n*(n+1) `div` 2

cost2 :: Int -> Int -> Int
cost2 x1 x2 = sum_triangle (distance x1 x2)

-- approximate mean
mean :: [Int] -> Int
mean xs = sum xs `div` length xs

solve2 :: Input -> Int
solve2 xs = min (total_cost cost2 xs m) (total_cost cost2 xs (m+1))
  where
    m = mean xs

tests2 :: [(String, Int)]
tests2 = [(testInput, 168)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
