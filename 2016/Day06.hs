module Main where

import Utilities
import Data.List

solve1 :: [String] -> String
solve1 = map (head . mostCommon) . transpose

testInput :: String
testInput =
    "eedadn\n\
    \drvtee\n\
    \eandsr\n\
    \raavrd\n\
    \atevrs\n\
    \tsrnev\n\
    \sdttsa\n\
    \rasrtv\n\
    \nssdts\n\
    \ntnada\n\
    \svetve\n\
    \tesnvt\n\
    \vntsnd\n\
    \vrdear\n\
    \dvrsen\n\
    \enarar\n"

tests1 :: [(String, String)]
tests1 = [(testInput, "easter")]

-- Part Two --

solve2 :: [String] -> String
solve2 = map (last . mostCommon) . transpose

tests2 :: [(String, String)]
tests2 = [(testInput, "advent")]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = lines s
    putStr (unlines (failures "solve1" (solve1 . lines) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . lines) tests2))
    putStrLn (solve2 input)
