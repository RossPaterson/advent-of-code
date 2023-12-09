module Main where

import Utilities

-- Input processing

type Input = [[Int]]

parse :: String -> Input
parse = map (map read . words) . lines

-- Part One

-- differences between successive elements of a sequence
diffs :: [Int] -> [Int]
diffs [] = []
diffs (n:ns) = zipWith (-) ns (n:ns)

-- successive differences while some are non-zero
all_diffs :: [Int] -> [[Int]]
all_diffs = takeWhile (any (/= 0)) . iterate diffs

-- extrapolate the sequence to the right
extrapolate_forwards :: [Int] -> Int
extrapolate_forwards = sum . map last . all_diffs

solve1 :: Input -> Int
solve1 = sum . map extrapolate_forwards

testInput :: String
testInput = "\
    \0 3 6 9 12 15\n\
    \1 3 6 10 15 21\n\
    \10 13 16 21 30 45\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 114)]

-- Part Two

-- extrapolate the sequence to the left
extrapolate_backwards :: [Int] -> Int
extrapolate_backwards = foldr (-) 0 . map head . all_diffs
-- alternative implementation:
-- extrapolate_backwards = extrapolate_forwards . reverse

solve2 :: Input -> Int
solve2 = sum . map extrapolate_backwards

tests2 :: [(String, Int)]
tests2 = [(testInput, 2)]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
