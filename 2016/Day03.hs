module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List

type Triple = [Int]
type Input = [Triple]

parse :: String -> Input
parse = map (runParser triple) . lines
  where
    triple = many space *> nat `sepBy1` some space

isTriangle :: [Int] -> Bool
isTriangle [x, y, z] = x+y > z && y+z > x && z+x > y
isTriangle _ = False

solve1 :: Input -> Int
solve1 = length . filter isTriangle

tests1 :: [(String, Int)]
tests1 = [("5 10 25", 0)]

-- Part Two --

verticalTriples :: Input -> Input
verticalTriples = concat . map (takes 3) . transpose

solve2 :: Input -> Int
solve2 = solve1 . verticalTriples

testInput2 :: String
testInput2 =
    "101 301 501\n\
    \102 302 502\n\
    \103 303 503\n\
    \201 401 601\n\
    \202 402 602\n\
    \203 403 603\n"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 6)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
