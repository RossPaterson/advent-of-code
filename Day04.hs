module Main where

import Utilities
import Data.List

type Input = [[String]]

parse :: String -> Input
parse = map words . lines

-- no repeated words
valid1 :: [String] -> Bool
valid1 xs = length (group (sort xs)) == length xs

solve1 :: Input -> Int
solve1 = length . filter valid1

tests1 :: [(String, Bool)]
tests1 = [
    ("aa bb cc dd ee", True),
    ("aa bb cc dd aa", False),
    ("aa bb cc dd aaa", True)]

-- Part Two

-- no word is an anagram of another
valid2 :: [String] -> Bool
valid2 = valid1 . map sort

solve2 :: Input -> Int
solve2 = length . filter valid2

tests2 :: [(String, Bool)]
tests2 = [
    ("abcde fghij", True),
    ("abcde xyz ecdab", False),
    ("a ab abc abd abf abj", True),
    ("iiii oiii ooii oooi oooo", True),
    ("oiii ioii iioi iiio", False)]

main :: IO ()
main = do
    s <- readFile "input04.txt"
    let input = parse s
    putStr (unlines (failures "valid1" (valid1 . words) tests1))
    print (solve1 input)
    putStr (unlines (failures "valid2" (valid2 . words) tests2))
    print (solve2 input)
