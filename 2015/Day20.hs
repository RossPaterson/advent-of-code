module Main where

import Number

solve1 :: Int -> Int
solve1 input = head [n | n <- [1..], 10*sumOfDivisors n >= toInteger input]

-- Part Two --

bigDivisors :: Int -> [Int]
bigDivisors n = [n `div` k | k <- [1..min n 50], n `mod` k == 0]

solve2 :: Int -> Int
solve2 input = head [n | n <- [1..], 11*sum (bigDivisors n) >= input]

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = read s
    print (solve1 input)
    print (solve2 input)
