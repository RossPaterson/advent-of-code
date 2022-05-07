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

-- given negative p, smallest non-negative t such that ypos v t <= p
-- (ceiling of larger solution of t^2/2 - (v + 1/2)*t + p = 0)
inv_ypos :: Int -> Int -> Int
inv_ypos v p = ceiling (v' + sqrt (v'*v' - 2*fromIntegral p))
  where
    v' = fromIntegral v + 1/2::Double

-- time interval for which y is in range, given initial vertical velocity v
time_range :: Range -> Int -> Range
time_range (lo, hi) v = (inv_ypos v hi, inv_ypos v (lo-1) - 1)

-- given p, smallest positive v such that xpos v t >= p
{-
xpos v t >= p
<=>
t <= v && v*t - t*(t-1)/2 >= p
||
t >= v && v*(v+1)/2 >= p

(1) v >= (p + t*(t-1)/2)/t && v >= t
i.e. v >= max t ((p + t*(t-1)/2 + t-1) `div` t)
-}
inv_xpos :: Int -> Int -> Int
inv_xpos p t =
    fromInteger $ bsearch $ \ v ->  v > 0 && xpos (fromInteger v) t >= p
--  | otherwise = undefined

-- initial velocities that will hit the target
solutions :: Input -> [(Int, Int)]
solutions (xr, yr) = [(x, y) |
    y <- [fst yr..1-fst yr],
    let (t_min, t_max) = time_range yr y,
    let ts = [t_min..t_max],
    t_min <= t_max,
--  x <- [1..snd xr], -- could be tighter
    x <- [inv_xpos (fst xr) t_max..inv_xpos (snd xr+1) t_min-1],
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
