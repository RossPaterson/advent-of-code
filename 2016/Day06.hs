module Main where

import Utilities
import Data.List

solve1 :: [String] -> String
solve1 = map (head . mostCommon) . transpose

-- Part Two --

solve2 :: [String] -> String
solve2 = map (last . mostCommon) . transpose

test1 = "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar\n"

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = lines s
    putStrLn (solve1 input)
    putStrLn (solve2 input)
