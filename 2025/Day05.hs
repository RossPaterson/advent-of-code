module Main where

import Parser
import Utilities
import Data.List (sortOn)

-- Input processing

type Input = ([Range], [Int])

-- pair with lo <= hi
type Range = (Int, Int)

parse :: String -> Input
parse s = case map lines (paragraphs s) of
    [p1, p2] -> (map (runParser range) p1, map (runParser nat) p2)
    _ -> error "input is not exactly two paragraphs"
  where
    range = (,) <$> nat <* char '-' <*> nat

-- Part One

solve1 :: Input -> Int
solve1 (rs, ns) = length $ filter (inranges rs) ns

inranges :: [Range] -> Int -> Bool
inranges rs n = or [inrange r n | r <- rs]

inrange :: Range -> Int -> Bool
inrange (lo, hi) n = lo <= n && n <= hi

testInput :: String
testInput = "\
    \3-5\n\
    \10-14\n\
    \16-20\n\
    \12-18\n\
    \\n\
    \1\n\
    \5\n\
    \8\n\
    \11\n\
    \17\n\
    \32\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 3)]

-- Part Two

solve2 :: Input -> Int
solve2 (rs, _) = sum $ map rangesize $ foldr addrange [] $ sortOn snd rs

rangesize :: Range -> Int
rangesize (lo, hi) = hi - lo + 1

-- Add a range with a smaller upper bound to a list of disjoint ranges
-- ordered by their upper bounds.
addrange :: Range -> [Range] -> [Range]
addrange r [] = [r]
addrange (lo1, hi1) ((lo2, hi2):rs)
  | hi1 < lo2 = (lo1, hi1):(lo2, hi2):rs
  | otherwise = (min lo1 lo2, hi2):rs

tests2 :: [(String, Int)]
tests2 = [(testInput, 14)]

main :: IO ()
main = do
    s <- readFile "input/05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
