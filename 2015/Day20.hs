module Main where

import Number

input :: Int
input = 29000000

solve1 :: Int
solve1 = head [n | n <- [1..], 10*sumOfFactors n >= input]

-- Part Two --

bigFactors :: Int -> [Int]
bigFactors n = [n `div` k | k <- [1..min n 50], n `mod` k == 0]

solve2 :: Int
solve2 = head [n | n <- [1..], 11*sum (bigFactors n) >= input]

main :: IO ()
main = do
    print solve1
    print solve2
