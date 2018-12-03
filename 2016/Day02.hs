module Main where

import Data.Char

data Direction = Direction Int Int
type Line = [Direction]
type Input = [Line]

getDirections :: String -> [Direction]
getDirections = foldr addDirection []

addDirection :: Char -> [Direction] -> [Direction]
addDirection 'L' ds = Direction (-1) 0:ds
addDirection 'R' ds = Direction 1 0:ds
addDirection 'U' ds = Direction 0 (-1):ds
addDirection 'D' ds = Direction 0 1:ds
addDirection _ ds = ds

parse :: String -> Input
parse = map getDirections . lines

data Position = Position Int Int

start :: Position
start = Position 1 1

move :: Position -> Direction -> Position
move (Position x y) (Direction dx dy)
  | inrange = Position x' y'
  | otherwise = Position x y
  where
    x' = x + dx
    y' = y + dy
    inrange = 0 <= x' && x' <= 2 && 0 <= y' && y' <= 2

code :: Position -> Char
code (Position x y) = chr (ord '1' + x + y*3)

addLine :: Position -> Line -> Position
addLine = foldl move

solve1 :: Input -> String
solve1 = map code . tail . scanl addLine start

-- Part Two

data Position2 = Position2 Int Int

start2 :: Position2
start2 = Position2 0 2

move2 :: Position2 -> Direction -> Position2
move2 (Position2 x y) (Direction dx dy)
  | inrange = Position2 x' y'
  | otherwise = Position2 x y
  where
    x' = x + dx
    y' = y + dy
    offset = abs (y'-2)
    inrange = x' - offset >= 0 && x' + offset <= 4

code2 :: Position2 -> Char
code2 (Position2 x y) = codes!!y!!x
  where codes = ["..1..", ".234.", "56789", ".ABC.", "..D.."]

addLine2 :: Position2 -> Line -> Position2
addLine2 = foldl move2

solve2 :: Input -> String
solve2 = map code2 . tail . scanl addLine2 start2

example = "ULL\nRRDDD\nLURDL\nUUUUD\n"

main :: IO ()
main = do
    s <- readFile "input02.txt"
    let input = parse s
    putStrLn (solve1 input)
    putStrLn (solve2 input)
