module Main where

import Utilities

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map read . lines

-- Part One

fuel :: Int -> Int
fuel m = m `div` 3 - 2

solve1 :: Input -> Int
solve1 = sum . map fuel

tests1 :: [(Int, Int)]
tests1 = [(12, 2), (14, 2), (1969, 654), (100756, 33583)]

-- Part Two

all_fuel :: Int -> Int
all_fuel = sum . takeWhile (>0) . tail . iterate fuel

solve2 :: Input -> Int
solve2 = sum . map all_fuel

tests2 :: [(Int, Int)]
tests2 = [(14, 2), (1969, 966), (100756, 50346)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "fuel" fuel tests1))
    print (solve1 input)
    putStr (unlines (failures "all_fuel" all_fuel tests2))
    print (solve2 input)
