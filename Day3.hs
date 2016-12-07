module Day3 where

import Parser
import Utilities
import Control.Applicative
import Data.List

type Triple = [Int]
type Input = [Triple]

parse :: String -> Input
parse = map getTriple . lines

getTriple :: String -> Triple
getTriple = runParser $ many space *> nat `sepBy1` some space

isTriangle :: [Int] -> Bool
isTriangle [x, y, z] = x+y > z && y+z > x && z+x > y
isTriangle _ = False

solve1 :: Input -> Int
solve1 = length . filter isTriangle

test1 = isTriangle [5, 10, 25]

puzzle1 = do
    s <- readFile "input3.txt"
    print (solve1 (parse s))

-- Part Two --

verticalTriples :: Input -> Input
verticalTriples = concat . map (groups 3) . transpose

solve2 :: Input -> Int
solve2 = solve1 . verticalTriples

test2 = verticalTriples (parse "101 301 501\n102 302 502\n103 303 503\n201 401 601\n202 402 602\n203 403 603\n")

puzzle2 = do
    s <- readFile "input3.txt"
    print (solve2 (parse s))
