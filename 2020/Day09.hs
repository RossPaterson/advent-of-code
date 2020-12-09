module Main where

import Utilities
import Data.List
import Data.Maybe

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map read . lines

-- Part One

-- x is the sum of two different elements of ys
sumOfTwo :: Int -> [Int] -> Bool
sumOfTwo x ys = or [elem (x-y) ys | y <- ys, x /= 2*y]

-- Drop the first n elements and successive elements that are the sum
-- of two different elements of the immediately preceding n elements.
dropValid :: Int -> [Int] -> [Int]
dropValid n xs =
    map fst $
    dropWhile (uncurry sumOfTwo) $
    -- pair each element with the preceding n elements
    zip (drop n xs) (map (take n) (tails xs))

part1 :: Int -> Input -> Int
part1 n = head . dropValid n

solve1 :: Input -> Int
solve1 = part1 25

testInput :: String
testInput = "\
    \35\n\
    \20\n\
    \15\n\
    \25\n\
    \47\n\
    \40\n\
    \62\n\
    \55\n\
    \65\n\
    \95\n\
    \102\n\
    \117\n\
    \150\n\
    \182\n\
    \127\n\
    \219\n\
    \299\n\
    \277\n\
    \309\n\
    \576\n"

tests1 :: [((Int, String), Int)]
tests1 = [((5, testInput), 127)]

-- Part Two

-- prefix of xs that sums to total, if any
prefixSum :: Int -> [Int] -> Maybe [Int]
prefixSum total xs
  | not (null front) && snd (last front) == total = Just (map fst front)
  | otherwise = Nothing
  where
    -- longest prefix of xs whose sum is <= total, paired with partial sums
    front = takeWhile (\ (_, s) -> s <= total) $ zip xs (scanl1 (+) xs)

-- contiguous subsequences (substrings) of xs that sum to total
-- (A linear implementation is also possible, keeping track of the start
-- and finish of the candidate subsequence, but is not required here.)
contiguousSum :: Int -> [Int] -> [[Int]]
contiguousSum total xs = mapMaybe (prefixSum total) (tails xs)

part2 :: Int -> Input -> Int
part2 n xs = minimum vs + maximum vs
  where
    vs = head (contiguousSum (part1 n xs) xs)

solve2 :: Input -> Int
solve2 = part2 25

tests2 :: [((Int, String), Int)]
tests2 = [((5, testInput), 62)]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "part1" (\ (n, t) -> part1 n (parse t)) tests1))
    print (solve1 input)
    putStr (unlines (failures "part2" (\ (n, t) -> part2 n (parse t)) tests2))
    print (solve2 input)
