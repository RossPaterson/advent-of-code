module Main where

import Data.List

type Input = [Int]

parse :: String -> Input
parse = map read . lines

total :: Int
total = 150

solve1 :: Input -> Int
solve1 = length . filter (== total) . map sum . subsequences

-- Part Two --

solve2 :: Input -> Int
solve2 sizes = length $ head $ group $ sort $
    [length cs | cs <- subsequences sizes, sum cs == total]

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
