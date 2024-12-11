module Main where

import Utilities
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = [Integer]

parse :: String -> Input
parse = map read . words

-- Part One

-- All that matters is how many of each stone we have, so use bags.

type Bag a = Map a Int

bagFromList :: Ord a => [a] -> Bag a
bagFromList xs = Map.fromListWith (+) [(x, 1) | x <- xs]

sizeBag :: Bag a -> Int
sizeBag = sum . Map.elems

concatMapBag :: (Ord a, Ord b) => (a -> [b]) -> Bag a -> Bag b
concatMapBag f xs =
    Map.fromListWith (+) [(y, n) | (x, n) <- Map.assocs xs, y <- f x]

solve1 :: Input -> Int
solve1 = sizeBag . times 25 step . bagFromList

step :: Bag Integer -> Bag Integer
step = concatMapBag change

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
solve2 = sizeBag . times 75 step . bagFromList

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
