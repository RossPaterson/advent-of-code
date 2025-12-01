module Main where

import Parser
import Utilities

import Control.Applicative
import Data.List

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = map (runParser turnDial) . lines
  where
    turnDial = negate <$ char 'L' <*> nat <|> id <$ char 'R' <*> nat

-- Part One

solve1 :: Input -> Int
solve1 = length . filter (== 0) . scanl turn 50

turn :: Int -> Int -> Int
turn a b = (a + b) `mod` 100

testInput :: String
testInput = "\
    \L68\n\
    \L30\n\
    \R48\n\
    \L5\n\
    \R60\n\
    \L55\n\
    \L1\n\
    \L99\n\
    \R14\n\
    \L82\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 3)]

-- Part Two

solve2 :: Input -> Int
solve2 = sum . snd . mapAccumL turn2 50

-- turn the dial, counting the zeroes we pass
turn2 :: Int -> Int -> (Int, Int)
turn2 a b = (turn a b, zeroes a b)

-- number of cumulative number is zero during this turn
zeroes :: Int -> Int -> Int
zeroes a b
  | b > 0 = (a+b) `div` 100
  | otherwise = (a+b) `div` (-100) - a `div` (-100)

tests2 :: [(String, Int)]
tests2 = [(testInput, 6)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
