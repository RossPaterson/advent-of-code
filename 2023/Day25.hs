module Main where

import Utilities
import Graph
import Parser
import Control.Applicative
import Data.List
import Data.Ord
import Data.Tuple
import Data.Map ((!))
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Wiring = [(Node, [Node])]
type Node = String

type Input = Wiring

parse :: String -> Input
parse = map (runParser connections) . lines
  where
    connections = (,) <$> component <* string ": " <*> sepBy1 component space
    component = some letter

-- Part One

type Edge = (Node, Node)

edges :: Wiring -> [Edge]
edges ws = [(c, c') | (c, cs) <- ws, c' <- cs]

-- Convert graph to Graphviz undirected graph format for rendering
graphToGV :: [Edge] -> String
graphToGV es =
    undirectedGV [(c, c', []) | (c, c') <- es]

undirectedGraph :: [Edge] -> FiniteGraph Node
undirectedGraph es = relation (es ++ map swap es)

-- shortest path between two nodes that does not use a direct edge between them
indirectPathLength :: FiniteGraph Node -> Edge -> Int
indirectPathLength g (c1, c2) =
    length $ takeWhile (notElem c2) $ bfs next [c1]
  where
    next c
      | c == c1 = Set.elems (Set.delete c2 (g!c))
      | c == c2 = Set.elems (Set.delete c1 (g!c))
      | otherwise = Set.elems (g!c)

-- remove an edge (or its reverse) from a list of edges
removeEdge :: [Edge] -> Edge -> [Edge]
removeEdge es e = filter (/= e) $ filter (/= (swap e)) es

test es = sortOn (Down . fst) [(indirectPathLength g e, e) | e <- es]
  where
    g = undirectedGraph es

-- The graph consists of two components connected by 3 edges.  Each of
-- the components has lots of internal connections, so that the bridging
-- edges are those it takes the most steps to bypass.
splitGraph :: [Edge] -> [Set Node]
splitGraph es =
    components $ relation $ foldl removeEdge es bridging_es
  where
    g = undirectedGraph es
    bridging_es = take 3 $ sortOn (Down . indirectPathLength g) es

solve1 :: Input -> Int
solve1 = product . map Set.size . splitGraph . edges

testInput :: String
testInput = "\
    \jqt: rhn xhk nvd\n\
    \rsh: frs pzl lsr\n\
    \xhk: hfx\n\
    \cmg: qnr nvd lhk bvb\n\
    \rhn: xhk bvb hfx\n\
    \bvb: xhk hfx\n\
    \pzl: lsr hfx nvd\n\
    \qnr: nvd\n\
    \ntq: jqt hfx bvb xhk\n\
    \nvd: lhk\n\
    \lsr: lhk\n\
    \rzs: qnr cmg lsr rsh\n\
    \frs: qnr lhk lsr\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 54)]

-- there is no Part Two on Day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
