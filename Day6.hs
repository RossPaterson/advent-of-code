module Day6 where

import Utilities
import Data.List

solve1 :: [String] -> String
solve1 = map (head . mostCommon) . transpose

-- Part Two --

solve2 :: [String] -> String
solve2 = map (last . mostCommon) . transpose

test1 = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar\n"

puzzle1 = do
    s <- readFile "input6.txt"
    putStrLn (solve1 (lines s))

puzzle2 = do
    s <- readFile "input6.txt"
    putStrLn (solve2 (lines s))
