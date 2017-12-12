module Main where

import Parser
import Utilities
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Node = Int
-- out-edges from each node
type Graph = Map Node [Node]
type Input = Graph

parse :: String -> Input
parse = Map.fromList . map (runParser node) . lines
  where
    node :: Parser (Int, [Int])
    node = (,) <$> nat <* string " <-> " <*> sepBy1 nat (string ", ")

-- nodes reachable from n
closure :: Node -> Graph -> [Node]
closure n g = concat (bfs (g!) [n])

solve1 :: Input -> Int
solve1 = length . closure 0

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
        Just (n, _) -> component:comps (Set.difference left component)
          where
            component = Set.fromList (closure n g)

solve2 :: Input -> Int
solve2 = length . components

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
