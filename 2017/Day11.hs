module Main where

import Geometry
import Utilities
import Data.Char

-- directions in an infinite hexagonal tiling, in counterclockwise order
data Direction = N | NW | SW | S | SE | NE
    deriving (Enum, Read, Show)

type Input = [Direction]

parse :: String -> Input
parse cs = read ("[" ++ map toUpper cs ++ "]")

-- one step in the hexagonal tiling, with North as reference direction
direction :: Direction -> HexCoord
direction = unitVector . fromEnum

solve1 :: Input -> Int
solve1 = norm . foldl (.+.) zero . map direction

tests1 :: [(String, Int)]
tests1 = [
    ("ne,ne,ne", 3),
    ("ne,ne,sw,sw", 0),
    ("ne,ne,s,s", 2),
    ("se,sw,se,sw,sw", 3)]

-- Part Two

solve2 :: Input -> Int
solve2 = maximum . map norm . scanl (.+.) zero . map direction

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
