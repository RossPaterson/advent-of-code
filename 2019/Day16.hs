module Main where

import Utilities
import Data.Char

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map digitToInt . filter isDigit

-- Part One

lastDigit :: Int -> Int
lastDigit n
  | n < 0 = (-n) `mod` 10
  | otherwise = n `mod` 10

applyPattern :: Int -> [Int] -> Int
applyPattern n xs =
    lastDigit $ sum $ zipWith (*) (tail (cycle p)) xs
  where
    p = concat [replicate n v | v <- [0, 1, 0, -1]]

phase :: [Int] -> [Int]
phase xs = [applyPattern i xs | i <- [1..length xs]]

solve1 :: Input -> String
solve1 = map intToDigit . take 8 . times 100 phase

tests1 :: [(String, String)]
tests1 = [
    ("80871224585914546619083218645595", "24176176"),
    ("19617804207202209144916044189917", "73745418"),
    ("69317163492948606335995924319873", "52432133")]

-- Part Two

-- Key insight: for the nth-last position, provided n is less than half
-- the length of the list, the pattern is
--
-- replicate (length xs - n) 0 ++ replicate n 1
--
-- so the new value is the sum (modulo 10) of the last n values.
--
-- It happens that the given input asks for something near the end of
-- the string.

-- addition modulo 10
add :: Int -> Int -> Int
add x y = (x+y) `mod` 10

-- On the last half of xs, phase2 xs agrees with phase xs.
phase2 :: [Int] -> [Int]
phase2 = scanr1 add

solve2 :: Input -> String
solve2 ns =
    map intToDigit $ take 8 $ times 100 phase2 $
        drop offset $ concat $ replicate 10000 ns
  where
    offset = read (map intToDigit (take 7 ns))

tests2 :: [(String, String)]
tests2 = [
    ("03036732577212944063491565474664", "84462026"),
    ("02935109699940807407585447034323", "78725270"),
    ("03081770884921959731165446850517", "53553731")]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStrLn (solve2 input)
