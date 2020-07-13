module Main where

import Number

input :: Int
input = 29000000

solve1 :: Int
solve1 = head [n | n <- [1..], 10*sumOfDivisors n >= toInteger input]

-- Part Two --

bigDivisors :: Int -> [Int]
bigDivisors n = [n `div` k | k <- [1..min n 50], n `mod` k == 0]

solve2 :: Int
solve2 = head [n | n <- [1..], 11*sum (bigDivisors n) >= input]

main :: IO ()
main = do
    print solve1
    print solve2
