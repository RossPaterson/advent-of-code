module Main where

import Utilities
import Data.List

type Input = [Int]

parse :: String -> Input
parse = map read . lines

solve1 :: Int -> Input -> Int
solve1 t = length . filter (== t) . map sum . subsequences

testInput :: String
testInput = "20\n15\n10\n5\n5\n"

tests1 :: [((Int, String), Int)]
tests1 = [((25, testInput), 4)]

-- Part Two --

solve2 :: Int -> Input -> Int
solve2 t sizes = length $ head $ group $ sort $
    [length cs | cs <- subsequences sizes, sum cs == t]

tests2 :: [((Int, String), Int)]
tests2 = [((25, testInput), 3)]

total :: Int
total = 150

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (uncurry solve1 . fmap parse) tests1))
    print (solve1 total input)
    putStr (unlines (failures "solve2" (uncurry solve2 . fmap parse) tests2))
    print (solve2 total input)
