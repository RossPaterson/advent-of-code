module Main where

import Utilities
import Data.Bag (Bag)
import qualified Data.Bag as Bag

-- Input processing

type Input = [Integer]

parse :: String -> Input
parse = map read . words

-- Part One

-- All that matters is how many of each stone we have, so use bags.

solve1 :: Input -> Int
solve1 = Bag.size . times 25 step . Bag.fromList

step :: Bag Integer -> Bag Integer
step = Bag.concatMap change

change :: Integer -> [Integer]
change n
  | n == 0 = [1]
  | even ndigits = [front, back]
  | otherwise = [2024*n]
  where
    (front, back) = n `divMod` (10^(ndigits `div` 2))
    ndigits = length (show n)

testInput :: String
testInput = "\
    \125 17\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 55312)]

-- Part Two

solve2 :: Input -> Int
solve2 = Bag.size . times 75 step . Bag.fromList

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
