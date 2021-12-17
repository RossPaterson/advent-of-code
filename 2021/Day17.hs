module Main where

import Utilities
import Parser

-- Input processing

type Range = (Int, Int)
type Input = (Range, Range)

parse :: String -> Input
parse = runParser bounds . head . lines
  where
    bounds :: Parser Input
    bounds = (,) <$ string "target area: x=" <*> range <* string ", y=" <*> range
    range :: Parser Range
    range = (,) <$> int <* string ".." <*> int

-- Part One

-- range utilities

inrange :: Range -> Int -> Bool
inrange (lo, hi) v = lo <= v && v <= hi

-- x and y components are independent functions of time

-- position at time t with initial velocity v and unit deceleration
pos :: Int -> Int -> Int
pos v t = v*t - t*(t-1) `div` 2

-- maximum pos occurs at t = v and t = v+1
max_pos :: Int -> Int
max_pos v = v*(v+1) `div` 2

-- xpos, assuming non-negative vx
xpos :: Int -> Int -> Int
xpos vx t
  | t < vx = pos vx t
  | otherwise = max_pos vx

ypos :: Int -> Int -> Int
ypos = pos

-- given negative p, smallest non-negative t such that pos v t <= p
inv_pos :: Int -> Int -> Int
inv_pos v p =
    fromInteger (bsearch (\ t -> t >= 0 && pos v (fromInteger t) <= p))

-- time interval for which y is in range, given initial vertical velocity v
time_range :: Range -> Int -> [Int]
time_range (lo, hi) v = [inv_pos v hi..inv_pos v (lo-1) - 1]

-- initial velocities that will hit the target
solutions :: Input -> [(Int, Int)]
solutions (xr, yr) = [(x, y) |
    y <- [fst yr..1-fst yr],
    let ts = time_range yr y,
    not (null ts),
    x <- [1..snd xr], -- could be tighter
    or [inrange xr (xpos x t) | t <- ts]]

solve1 :: Input -> Int
solve1 rs = maximum [max_pos y | (_, y) <- solutions rs, y > 0]

testInput :: String
testInput = "target area: x=20..30, y=-10..-5"

tests1 :: [(String, Int)]
tests1 = [(testInput, 45)]

-- Part Two

solve2 :: Input -> Int
solve2 = length . solutions

tests2 :: [(String, Int)]
tests2 = [(testInput, 112)]

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
