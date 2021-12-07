module Main where

import Utilities

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = readNumbers

-- Part One

distance :: Int -> Int -> Int
distance x1 x2 = abs (x1 - x2)

least_total_cost :: (Int -> Int -> Int) -> [Int] -> Int
least_total_cost cost xs =
    minimum [sum (map (cost v) xs) | v <- [minimum xs..maximum xs]]

solve1 :: Input -> Int
solve1 = least_total_cost distance

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

solve2 :: Input -> Int
solve2 = least_total_cost cost2

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
