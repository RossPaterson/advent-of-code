module Main where

import Geometry
import Graph
import Utilities
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = TopoMap

type TopoMap = Map Position Int

parse :: String -> Input
parse s = Map.fromList [(p, digitToInt c) | (p, c) <- readGrid s]

-- Part One

-- all trails start at height 0
starts :: TopoMap -> [Position]
starts m = [p | p <- Map.keys m, Map.lookup p m == Just 0]

-- all trails end at height 9
finish :: TopoMap -> Position -> Bool
finish m p = Map.lookup p m == Just 9

-- each step on a trail goes up by at most 1
step :: TopoMap -> Position -> [Position]
step m p =
    [p' | d <- unitVectors, let p' = p .+. d, Map.lookup p' m == Just h]
  where
    h = m!p + 1

solve1 :: Input -> Int
solve1 m = sum $ map (score m) $ starts m

-- number of finishes reachable from p
score :: TopoMap -> Position -> Int
score m p = length [p' | p' <- concat (bfs (step m) [p]), finish m p']

testInput :: String
testInput = "\
    \89010123\n\
    \78121874\n\
    \87430965\n\
    \96549874\n\
    \45678903\n\
    \32019012\n\
    \01329801\n\
    \10456732\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 36)]

-- Part Two

solve2 :: Input -> Int
solve2 m = sum $ map (rating m) $ starts m

-- number of distinct paths starting at p
rating :: TopoMap -> Position -> Int
rating m p
  | finish m p = 1
  | otherwise = sum [rating m p' | p' <- step m p]

tests2 :: [(String, Int)]
tests2 = [(testInput, 81)]

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
