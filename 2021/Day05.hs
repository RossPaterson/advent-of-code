module Main where

import Utilities
import Geometry
import Parser

-- Input processing

data Line = Line Point2 Point2
    deriving Show

type Input = [Line]

parse :: String -> Input
parse = map (runParser line) . lines
  where
    line = Line <$> position <* string " -> " <*> position
    position = Point2 <$> nat <* char ',' <*> nat

-- Part One

-- list of numbers from x to y in increments or decrements of 1
range :: Int -> Int -> [Int]
range x y
  | x < y = [x..y]
  | otherwise = [x,x-1..y]

-- all the points on a horizontal or vertical line
points :: Line -> [Point2]
points (Line (Point2 x1 y1) (Point2 x2 y2))
  | x1 == x2 = [Point2 x1 y | y <- range y1 y2]
  | y1 == y2 = [Point2 x y1 | x <- range x1 x2]
  | otherwise = []

-- number of points that occur on more than one horizontal or vertical line
solve1 :: Input -> Int
solve1 = length . filter ((> 1) . snd) . frequency . concat . map points

testInput :: String
testInput = "\
    \0,9 -> 5,9\n\
    \8,0 -> 0,8\n\
    \9,4 -> 3,4\n\
    \2,2 -> 2,1\n\
    \7,0 -> 7,4\n\
    \6,4 -> 2,0\n\
    \0,9 -> 2,9\n\
    \3,4 -> 1,4\n\
    \0,0 -> 8,8\n\
    \5,5 -> 8,2\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 5)]

-- Part Two

-- all the points on a horizontal, vertical or diagonal line
points2 :: Line -> [Point2]
points2 (Line (Point2 x1 y1) (Point2 x2 y2))
  | abs (x2-x1) == abs (y2-y1) = zipWith Point2 (range x1 x2) (range y1 y2)
points2 line = points line

-- number of points that occur on more than one horizontal, vertical or
-- diagonal line
solve2 :: Input -> Int
solve2 = length . filter ((> 1) . snd) . frequency . concat . map points2

tests2 :: [(String, Int)]
tests2 = [(testInput, 12)]

main :: IO ()
main = do
    s <- readFile "input/05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
