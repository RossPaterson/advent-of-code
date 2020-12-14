module Main where

import Geometry
import Graph
import Parser
import Utilities
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = [Point4]

parse :: String -> Input
parse = map (runParser point . dropWhile (== ' ')) . lines
  where
    point = Point4 <$> int <* comma <*> int <* comma <*> int <* comma <*> int
    comma = char ','

-- Part One

-- number of constellations in the input
solve1 :: Input -> Int
solve1 ps = length $ components (neighbours point_map) (Map.keysSet point_map)
  where
    point_map = Map.fromList (zip [0..] ps)

-- list of points connected to a point
neighbours :: Map Int Point4 -> Int -> [Int]
neighbours m n = case Map.lookup n m of
    Nothing -> []
    Just p -> [n' | (n', p') <- Map.toList m, near p p' && n' /= n]

-- two points are connected if their distance is no more than 3
near :: Point4 -> Point4 -> Bool
near p1 p2 = distance p1 p2 <= 3

tests1 :: [(String, Int)]
tests1 = [
    (" 0,0,0,0\n\
     \ 3,0,0,0\n\
     \ 0,3,0,0\n\
     \ 0,0,3,0\n\
     \ 0,0,0,3\n\
     \ 0,0,0,6\n\
     \ 9,0,0,0\n\
     \12,0,0,0\n", 2),
    ("-1,2,2,0\n\
     \0,0,2,-2\n\
     \0,0,0,-2\n\
     \-1,2,0,0\n\
     \-2,-2,-2,2\n\
     \3,0,2,-1\n\
     \-1,3,2,2\n\
     \-1,0,-1,0\n\
     \0,2,1,-2\n\
     \3,0,0,0\n", 4),
    ("1,-1,0,1\n\
     \2,0,-1,0\n\
     \3,2,-1,0\n\
     \0,0,3,1\n\
     \0,0,-1,-1\n\
     \2,3,-2,0\n\
     \-2,2,0,0\n\
     \2,-2,0,-1\n\
     \1,-1,0,-1\n\
     \3,2,0,2\n", 3),
    ("1,-1,-1,-2\n\
     \-2,-2,0,1\n\
     \0,2,1,3\n\
     \-2,3,-2,1\n\
     \0,2,3,-2\n\
     \-1,-1,1,-2\n\
     \0,-2,-1,0\n\
     \-2,2,3,-1\n\
     \1,2,2,0\n\
     \-1,-2,0,-2\n", 8)]

-- there is no Part Two on Day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
