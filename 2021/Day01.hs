module Main where

import Utilities
import Data.List

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = readNumbers

-- Part One

-- number of times an element is less than the immediately following element
increases :: (Ord a) => [a] -> Int
increases vs = length $ filter id $ zipWith (<) vs (tail vs)

solve1 :: Input -> Int
solve1 = increases

testInput :: String
testInput = "\
    \199\n\
    \200\n\
    \208\n\
    \210\n\
    \200\n\
    \207\n\
    \240\n\
    \269\n\
    \260\n\
    \263\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 7)]

-- Part Two

-- number of times the sum of a 3-element window increases
solve2 :: Input -> Int
solve2 = increases . map (sum . take 3) . tails

tests2 :: [(String, Int)]
tests2 = [(testInput, 5)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
