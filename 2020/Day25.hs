module Main where

import Utilities
import Number

-- Input processing

type Input = (Int, Int)

parse :: String -> Input
parse s = case map read (lines s) of
    [x, y] -> (x, y)
    _ -> error "bad input"

-- Part One

modulus :: Int
modulus = 20201227

base :: Int
base = 7

-- Diffie-Hellman key exchange:
-- From a modulus and base, and values d and c satisfying
--
--     d == base ^ log_d (mod modulus)
--     c == base ^ log_c (mod modulus)
--
-- find the value c ^ log_d = base ^ (log_d*log_c) = d ^ log_c.
solve1 :: Input -> Int
solve1 (d, c) = case modularLogarithm modulus base d of
    Just log_d -> modularPower modulus c log_d
    Nothing -> error "no solution"

testInput :: String
testInput = "\
    \5764801\n\
    \17807724\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 14897079)]

-- there is no Part Two on Day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
