module Main where

import Utilities

-- Input processing

type Input = (Int, Int)

parse :: String -> Input
parse s = case readNumbers s of
    [row, col] -> (row, col)
    _ -> error "bad input"

-- Part One

-- position in an infinite sequence of (row, column) coordinates in the
-- diagonalization
index :: Int -> Int -> Int
index r c = before + c
  where
    diagonal = r + c - 1
    before = diagonal*(diagonal-1) `div` 2

-- strict version of iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : (iterate' f $! f x)

numbers :: [Integer]
numbers = iterate' step 20151125
  where
    step n = n * 252533 `mod` 33554393::Integer

solve1 :: (Int, Int) -> Integer
solve1 (r, c) = numbers !! (index r c - 1)

-- no Part Two on day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    print (solve1 input)
