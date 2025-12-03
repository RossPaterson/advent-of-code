module Main where

import Utilities
import Data.Char
import Data.List

-- Input processing

type Input = [[Int]]

parse :: String -> Input
parse = map (map digitToInt) . lines

-- Part One

solve1 :: Input -> Int
solve1 = sum . map (maxSubNumber 2)

-- original implementation: largest number that can be formed from two
-- digits of ds, in their original order
maxSubNumber2 :: [Int] -> Int
maxSubNumber2 ds = maximum [d1*10 + d2 | d1:rest <- tails ds, d2 <- rest]

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
maxSubNumber :: Int -> [Int] -> Int
maxSubNumber k ds = maxSubNumbers ds !! (k-1)

-- The kth element of the output list is the largest number that can be
-- formed by selecting k digits from ds, in their original order.
-- We calculate this for each tail of the original list, starting from
-- the empty tail and proceeding to the full list.
maxSubNumbers :: [Int] -> [Int]
maxSubNumbers ds = foldr addDigit [] ds

-- add (or not) a single digit to a list of best subnumbers for the
-- following digits
addDigit :: Int -> [Int] -> [Int]
addDigit d best =
    zipWith max (best++[0]) (zipWith (+) (iterate (*10) d) (0:best))

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
