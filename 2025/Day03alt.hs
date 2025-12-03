-- greedy solution
module Main where

import Utilities
import Data.Function
import Data.List

-- Input processing

type Input = [[Char]]

parse :: String -> Input
parse = lines

-- Part One

solve1 :: Input -> Int
solve1 = sum . map (maxSubNumber 2)

testInput :: String
testInput = "\
    \987654321111111\n\
    \811111111111119\n\
    \234234234234278\n\
    \818181911112111\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 357)]

-- Part Two

solve2 :: Input -> Int
solve2 = sum . map (maxSubNumber 12)

-- the largest number that can be formed by selecting k digits from ds,
-- in their original order
maxSubNumber :: Int -> [Char] -> Int
maxSubNumber k ds = read (bestDigits k ds)

-- Assuming that length ds >= k, the k digits from ds making the largest
-- number can be computed greedily, by noting that:
--
--  * the first digit of the best sequence is the largest digit that has
--    at least k-1 more digits after it, and
--
--  * the earliest such digit gives the largest set of digits from which
--    the other k-1 can be chosen.
--
bestDigits :: Ord a => Int -> [a] -> [a]
bestDigits k ds
  | k <= 0 = []
  | otherwise =
    case maximumBy (lessOrFirst `on` fst) [(d, rest) | d:rest <- longTails] of
        (d, rest) -> d : bestDigits (k-1) rest
  where
    -- tails with at least k elements
    longTails = take (length ds - k + 1) (tails ds)

-- like compare, but EQ is treated as GT (so max chooses the first)
lessOrFirst :: Ord a => a -> a -> Ordering
lessOrFirst x y = case compare x y of
    LT -> LT
    _ -> GT

tests2 :: [(String, Int)]
tests2 = [(testInput, 3121910778619)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
