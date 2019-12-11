module Main where

import Parser
import Data.List

-- Input processing

type Input = Range

type Range = (Int, Int)

parse :: String -> Input
parse = runParser range . head . lines
  where
    range = (,) <$> nat <* char '-' <*> nat

-- Part One

numDigits :: Range -> Int
numDigits (_, h) = length (show h)

inRange :: Range -> Int -> Bool
inRange (l, h) n = l <= n && n <= h

-- has a pair of equal adjacent values
hasPair :: Eq a => [a] -> Bool
hasPair xs = or (zipWith (==) xs (tail xs))

-- all non-decreasing lists of n values from [a..b]
-- The number of lists is the binomial coefficient C^(n+b-a)_n
nonDec :: Enum a => a -> a -> Int -> [[a]]
nonDec a b n
  | n == 0 = [[]]
  | otherwise = [next:rest | next <- [a..b], rest <- nonDec next b (n-1)]

solve1 :: Input -> Int
solve1 r = length $
    filter (inRange r) $ map read $
    filter hasPair $
    nonDec '0' '9' (numDigits r)

-- Part Two

-- has a pair of equal adjacent values not part of a larger group
justPair :: Eq a => [a] -> Bool
justPair xs = 2 `elem` map length (group xs)

solve2 :: Input -> Int
solve2 r = length $
    filter (inRange r) $ map read $
    filter justPair $
    nonDec '0' '9' (numDigits r)

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
