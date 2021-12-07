module Main where

import Utilities

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = readNumbers

-- Part One

distances :: Int -> [Int] -> [Int]
distances v xs = [abs (x - v) | x <- xs]

move_cost1 :: [Int] -> Int -> Int
move_cost1 xs v = sum (distances v xs)

minimize :: (Enum a, Ord a, Ord b) => (a -> b) -> [a] -> b
minimize f xs = minimum (map f [minimum xs..maximum xs])

solve1 :: Input -> Int
solve1 xs = minimize (move_cost1 xs) xs

testInput :: String
testInput = "16,1,2,0,4,2,7,1,2,14"

tests1 :: [(String, Int)]
tests1 = [(testInput, 37)]

-- Part Two

sum_triangle :: Int -> Int
sum_triangle n = n*(n+1) `div` 2

move_cost2 :: [Int] -> Int -> Int
move_cost2 xs v = sum (map sum_triangle (distances v xs))

solve2 :: Input -> Int
solve2 xs = minimize (move_cost2 xs) xs

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
