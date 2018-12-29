module Main where

import Parser
import Utilities
import Data.Map (Map)
import qualified Data.Map as Map

type Layer = (Int, Int)
type Wall = [Layer]
type Input = Wall

parse :: String -> Input
parse = map (runParser layer) . lines
  where
    layer = (,) <$> nat <* string ": " <*> nat

-- Layers whose scanners are in the start position when a packet started
-- after delay arrives at their depth.
catches :: Int -> Wall -> [Layer]
catches delay = filter caught
  where
    -- The period of a back-and-forth scanner with range r is 2*(r-1)
    caught :: Layer -> Bool
    caught (d, r) = (d + delay) `mod` (2*(r-1)) == 0

severity :: Layer -> Int
severity (d, r) = d*r

solve1 :: Input -> Int
solve1 = sum . map severity . catches 0

testInput :: String
testInput =
    "0: 3\n\
    \1: 2\n\
    \4: 4\n\
    \6: 4\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 24)]

-- Part Two

safeDelays :: Input -> [Int]
safeDelays wall = [delay | delay <- [0..], null (catches delay wall)]

solve2 :: Input -> Int
solve2 = head . safeDelays

tests2 :: [(String, Int)]
tests2 = [(testInput, 10)]

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
