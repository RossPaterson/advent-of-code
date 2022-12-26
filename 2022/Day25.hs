module Main where

import Utilities
import Data.Char
import Data.Tuple

-- Input processing

type Input = [Int]

base :: Int
base = 5

parse :: String -> Input
parse = map readNumber . lines

-- Base 5, with digits '=' (-1), '-' (-1), '0'..'2'
readNumber :: String -> Int
readNumber = foldl addDigit 0
  where
    addDigit n d = n*base + readDigit d

readDigit :: Char -> Int
readDigit c
  | c == '=' = -2
  | c == '-' = -1
  | '0' <= c && c <= '2' = ord c - ord '0'
  | otherwise = error "bad digit"

-- Part One

showNumber :: Int -> String
showNumber n
  | q == 0 = [showDigit n]
  | otherwise = showNumber q ++ [showDigit r]
  where
    -- q*base + r = n
    q = (n+2) `div` base
    r = (n+2) `mod` base - 2

showDigit :: Int -> Char
showDigit n
  | n == -2 = '='
  | n == -1 = '-'
  | 0 <= n && n <= 2 = chr (ord '0' + n)
  | otherwise = error "bad number"

solve1 :: Input -> String
solve1 = showNumber . sum

sample :: [(Int, String)]
sample = [
    (1, "1"),
    (2, "2"),
    (3, "1="),
    (4, "1-"),
    (5, "10"),
    (6, "11"),
    (7, "12"),
    (8, "2="),
    (9, "2-"),
    (10, "20"),
    (15, "1=0"),
    (20, "1-0"),
    (2022, "1=11-2"),
    (12345, "1-0---0"),
    (314159265, "1121-1110-1=0"),
    (1747, "1=-0-2"),
    (906, "12111"),
    (198, "2=0="),
    (11, "21"),
    (201, "2=01"),
    (31, "111"),
    (1257, "20012"),
    (32, "112"),
    (353, "1=-1="),
    (107, "1-12"),
    (7, "12"),
    (3, "1="),
    (37, "122")]

testInput :: String
testInput = "\
    \1=-0-2\n\
    \12111\n\
    \2=0=\n\
    \21\n\
    \2=01\n\
    \111\n\
    \20012\n\
    \112\n\
    \1=-1=\n\
    \1-12\n\
    \12\n\
    \1=\n\
    \122\n"

tests1 :: [(String, String)]
tests1 = [(testInput, "2=-1=0")]

-- there is no Part Two on Day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStr (unlines (failures "readNumber" readNumber (map swap sample)))
    putStr (unlines (failures "showNumber" showNumber sample))
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStr (solve1 input)
