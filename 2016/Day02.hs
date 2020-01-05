module Main where

import Utilities
import Cartesian
import Parser
import Control.Applicative
import Data.Char

data Direction = L | R | U | D
    deriving (Bounded, Enum, Show)
type Line = [Direction]
type Input = [Line]

parse :: String -> Input
parse = map (runParser line) . lines
  where
    line = some enumValue

-- Part One

direction :: Direction -> Point2
direction L = Point2 (-1) 0
direction R = Point2 1 0
direction U = Point2 0 (-1)
direction D = Point2 0 1

-- square 3x3 keypad, centred on the origin
-- 1 2 3
-- 4 5 6
-- 7 8 9

inrange :: Point2 -> Bool
inrange (Point2 x y) = abs x <= 1 && abs y <= 1

code :: Point2 -> Char
code (Point2 x y) = chr (ord '1' + x + y*3 + 4)

-- start on the 5
start :: Point2
start = zero

move :: Point2 -> Direction -> Point2
move p d
  | inrange p' = p'
  | otherwise = p
  where
    p' = p .+. direction d

addLine :: Point2 -> Line -> Point2
addLine = foldl move

solve1 :: Input -> String
solve1 = map code . tail . scanl addLine start

testInput :: String
testInput =
    "ULL\n\
    \RRDDD\n\
    \LURDL\n\
    \UUUUD\n"

tests1 :: [(String, String)]
tests1 = [(testInput, "1985")]

-- Part Two

-- diamond-shaped keypad, centred on the origin
--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D

inrange2 :: Point2 -> Bool
inrange2 p = norm p <= 2

code2 :: Point2 -> Char
code2 (Point2 x y) = codes!!(y+2)!!(x+2)
  where
    codes = ["..1..", ".234.", "56789", ".ABC.", "..D.."]

-- start on the 5
start2 :: Point2
start2 = Point2 (-2) 0

move2 :: Point2 -> Direction -> Point2
move2 p d
  | inrange2 p' = p'
  | otherwise = p
  where
    p' = p .+. direction d

addLine2 :: Point2 -> Line -> Point2
addLine2 = foldl move2

solve2 :: Input -> String
solve2 = map code2 . tail . scanl addLine2 start2

tests2 :: [(String, String)]
tests2 = [(testInput, "5DB3")]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStrLn (solve2 input)
