-- alternate version taking time O(log n), but with a large constant factor
module Main where

import Utilities
import Matrix
import qualified Data.Map as Map

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = readNumbers

-- Part One

-- number of fish with internal timer values of 0..8
-- (all fish with the same counter are identical)
counts :: [Int] -> [Integer]
counts ns = [Map.findWithDefault 0 i m | i <- [0..8]]
  where
    m = Map.fromListWith (+) [(n, 1) | n <- ns]

-- daily linear transformation of the numbers of fish with each internal
-- timer value
lanternfish :: Matrix Integer
lanternfish = Matrix [
    [0, 1, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 1],
    [1, 0, 0, 0, 0, 0, 0, 0, 0]]

solve1 :: Input -> Integer
solve1 = sum . apply (lanternfish^(80::Int)) . counts

testInput :: String
testInput = "3,4,3,1,2"

tests1 :: [(String, Integer)]
tests1 = [(testInput, 5934)]

-- Part Two

solve2 :: Input -> Integer
solve2 = sum . apply (lanternfish^(256::Int)) . counts

tests2 :: [(String, Integer)]
tests2 = [(testInput, 26984457539)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
