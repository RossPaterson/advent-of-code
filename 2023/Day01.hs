module Main where

import Utilities
import Data.Char
import Data.List
import Data.Maybe

-- Input processing

type Input = [String]

parse :: String -> Input
parse = lines

-- Part One

calibration_value :: String -> Int
calibration_value s = head digits*10 + last digits
  where
    digits = map digitToInt (filter isDigit s)

solve1 :: Input -> Int
solve1 = sum . map calibration_value

testInput :: String
testInput = "\
    \1abc2\n\
    \pqr3stu8vwx\n\
    \a1b2c3d4e5f\n\
    \treb7uchet\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 142)]

-- Part Two

digit_names :: [String]
digit_names =
    ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

-- all the digits (possibly overlapping) in the string
get_digits :: String -> [Int]
get_digits = catMaybes . map get_digit . tails

-- the digit (char or word) at the start of the string, if any
get_digit :: String -> Maybe Int
get_digit (c:_)
  | isDigit c = Just (digitToInt c)
get_digit s =
    listToMaybe [n | (n, name) <- zip [1..] digit_names, isPrefixOf name s]

calibration_value2 :: String -> Int
calibration_value2 s = head digits*10 + last digits
  where
    digits = get_digits s

solve2 :: Input -> Int
solve2 = sum . map calibration_value2

testInput2 :: String
testInput2 = "\
    \two1nine\n\
    \eightwothree\n\
    \abcone2threexyz\n\
    \xtwone3four\n\
    \4nineeightseven2\n\
    \zoneight234\n\
    \7pqrstsixteen\n"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 281)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
