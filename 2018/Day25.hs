module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Char
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Point]

parse :: String -> Input
parse = map (runParser point . dropWhile (== ' ')) . lines
  where
    point = (,,,) <$> int <* comma <*> int <* comma <*> int <* comma <*> int
    comma = char ','

type Point = (Int, Int, Int, Int)

-- Part One

-- number of constellations in the input
solve1 :: Input -> Int
solve1 = length . components . neighbourhoodGraph . Map.fromList . zip [0..]

-- a directed graph, represented by a list of neighbours for each node
-- (our graphs are indirected: they include the reverse of each edge)
type Graph a = Map a [a]

-- list of points connected to each point
neighbourhoodGraph :: Map Int Point -> Graph Int
neighbourhoodGraph ps = Map.mapWithKey neighbours ps
  where
    neighbours n1 p1 = [n2 | (n2, p2) <- Map.toList ps, near p1 p2 && n1 /= n2]

-- two points are connected if their distance is no more than 3
near :: Point -> Point -> Bool
near p1 p2 = distance p1 p2 <= 3

-- Manhattan distance
distance :: Point -> Point -> Int
distance (x1, y1, z1, t1) (x2, y2, z2, t2) =
    abs (x1-x2) + abs (y1-y2) + abs (z1-z2) + abs (t1-t2)

-- list of connected components of a graph
components :: Ord a => Graph a -> [Set a]
components = unfoldr extractComponent

-- extract a non-empty connected component from the graph
extractComponent :: Ord a => Graph a -> Maybe (Set a, Map a [a])
extractComponent neighbours =
    fmap (extract . fst) (Map.lookupMin neighbours)
  where
    extract p = (component, Map.withoutKeys neighbours component)
      where
        component = Set.fromList $ concat $ bfs (neighbours!) [p]

tests1 :: [(String, Int)]
tests1 =
    [(" 0,0,0,0\n\
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
    s <- readFile "input25.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
