module Main where

import Utilities
import Data.Char

-- Input processing

type Input = [Race]
type Race = (Int, Int)

parse :: String -> Input
parse s = zip (head ls) (head (tail ls))
  where
    ls = map (map read . tail . words) (lines s)

-- Part One

-- total distance travelled after holding for hold seconds
race :: Int -> Int -> Int
race total hold = (total - hold)*hold

-- number of values 0 <= t <= time such that race time t > dist
wins :: Race -> Int
wins (time, dist) =
    length [hold | hold <- [1..time-1], race time hold > dist]

solve1 :: Input -> Int
solve1 = product . map wins

testInput :: String
testInput = "\
    \Time:      7  15   30\n\
    \Distance:  9  40  200\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 288)]

-- Part Two

-- This time it's just two numbers
parse2 :: String -> Race
parse2 s = (head ls, head (tail ls))
  where
    ls = map (read . filter isDigit) (lines s)

-- number of values 0 <= t <= time such that time*t - t*t - dist > 0
-- using the quadratic formula
wins2 :: Race -> Int
wins2 (time, dist) = ceiling (b2+d) - floor (b2-d) - 1
  where
    b2 = fromIntegral time/2::Double
    d = sqrt (b2^(2::Int) - fromIntegral dist)

solve2 :: Race -> Int
solve2 = wins2

tests2 :: [(String, Int)]
tests2 = [(testInput, 71503)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    let input2 = parse2 s
    putStr (unlines (failures "solve2" (solve2 . parse2) tests2))
    print (solve2 input2)
