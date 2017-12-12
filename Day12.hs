module Main where

import Parser
import Utilities
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Node = Int
type Input = [(Node, [Node])]

parse :: String -> Input
parse = map (runParser node) . lines
  where
    node = (,) <$> nat <* string " <-> " <*> sepBy1 nat (string ", ")

-- out-edges from each node
type Graph = Map Node [Node]

emptyGraph :: Graph
emptyGraph = Map.empty

-- add an edge from x to y
addEdge :: Node -> Node -> Graph -> Graph
addEdge x y g = case Map.lookup x g of
    Nothing -> Map.insert x [y] g
    Just ys -> Map.insert x (y:ys) g

-- create a bidirectional graph
bigraph :: Input -> Graph
bigraph xys =
    compose [addEdge x y . addEdge y x | (x, ys) <- xys, y <- ys] emptyGraph

-- nodes connected to n
closure :: Node -> Graph -> [Node]
closure n g = concat (bfs (g!) [n])

solve1 :: Input -> Int
solve1 = length . closure 0 . bigraph

testInput =
    "0 <-> 2\n\
    \1 <-> 1\n\
    \2 <-> 0, 3, 4\n\
    \3 <-> 2, 4\n\
    \4 <-> 2, 3, 6\n\
    \5 <-> 6\n\
    \6 <-> 4, 5\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 6)]

-- Part Two

-- connected components of the graph
components :: Graph -> [Set Node]
components g = comps (Map.keysSet g)
  where
    comps left = case Set.minView left of
        Nothing -> []
        Just (n, _) -> reached:comps (Set.difference left reached)
          where
            reached = Set.fromList (closure n g)

solve2 :: Input -> Int
solve2 = length . components . bigraph

tests2 :: [(String, Int)]
tests2 = [(testInput, 2)]

main :: IO ()
main = do
    s <- readFile "input12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
