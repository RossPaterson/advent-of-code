module Main where

import Utilities
import Graph
import Parser
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree

-- Input processing

type Input = Valves
type Valves = Map ValveName Valve
type ValveName = String
data Valve = Valve { flow_rate :: Int, exits :: [ValveName] }
    deriving (Show)

parse :: String -> Input
parse = Map.fromList . map (runParser named_valve) . lines
  where
    named_valve = (,) <$ string "Valve " <*> name <*> valve
    valve = Valve <$ string " has flow rate=" <*> nat <* string "; " <*
        string_s "tunnel" <* space <* string_s "lead" <* string " to " <*
        string_s "valve" <* space <*> sepBy1 name (string ", ")
    string_s s = string s <* (pure 's' <|> char 's')
    name = (\ a b -> [a, b]) <$> letter <*> letter

-- Part One

-- Special valves

start :: ValveName
start = "AA"

-- flow rates of working valves
flowRates :: Valves -> Map ValveName Int
flowRates vs = Map.filter (> 0) (Map.map flow_rate vs)

-- Weighted graph of working valves

type WeightedGraph a = Map a (Map a Int)

-- edges from the start and working valves to other working valves
-- The edge cost is the number of moves plus one to turn the valve on.
valveGraph :: Valves -> WeightedGraph ValveName
valveGraph vs =
    Map.fromList [(source, out_edges source) | source <- sources]
  where
    flow = flowRates vs
    sources = start : Map.keys flow
    out_edges source = Map.fromList [(target, dist) |
        (dist, targets) <- tail $ zip [1..] $ bfs (exits . (vs !)) [source],
        target <- targets, Map.member target flow]

-- The maximum flow of all possible tours of working valves within
-- the time limit
maxFlow :: Map ValveName Int -> WeightedGraph ValveName -> Int -> Int
maxFlow flow g time_limit =
    foldr max 0 $
        concat $
        map (flatten . scanTree (+) 0 . fmap valve_flow) $
        reachableValves g time_limit $
        Map.keys flow
  where
    valve_flow (t, n) = t * flow!n

-- forest of valves that can be opened within the time limit,
-- with how long they will be open
reachableValves :: WeightedGraph ValveName -> Int -> [ValveName] ->
    Forest (Int, ValveName)
reachableValves g time_limit ns =
    takeWhileForest (\ (t, _) -> t > 0) $
            -- forest of valve opening times and locations
        scanForest advance (time_limit, start) $
            -- forest of nodes
        permutationForest ns
  where
    advance (t, n1) n2 = (t - g!n1!n2, n2)

solve1 :: Input -> Int
solve1 vs = maxFlow (flowRates vs) (valveGraph vs) time_limit
  where
    time_limit = 30

testInput :: String
testInput = "\
    \Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
    \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
    \Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
    \Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
    \Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
    \Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
    \Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
    \Valve HH has flow rate=22; tunnel leads to valve GG\n\
    \Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
    \Valve JJ has flow rate=21; tunnel leads to valve II\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 1651)]

-- Part Two

-- For each set of valves that can be reached within the time limit,
-- the maximum flow they can produce
maxFlows :: Map ValveName Int -> WeightedGraph ValveName -> Int ->
    [(Set ValveName, Int)]
maxFlows flow g time_limit =
    Map.assocs $
        Map.fromListWith max $
        concat $
        map (flatten . scanTree add (Set.empty, 0)) $
        reachableValves g time_limit (Map.keys flow)
  where
    add (ns, f) (t, n) = (Set.insert n ns, f + t*flow!n)

-- In this part, we have two independent actors turning on disjoint sets
-- of valves.
solve2 :: Input -> Int
solve2 vs =
    maximum [f1 + f2 |
        (ns1, f1) <- flows, (ns2, f2) <- flows, Set.disjoint ns1 ns2]
  where
    time_limit = 26
    flows = maxFlows (flowRates vs) (valveGraph vs) time_limit

tests2 :: [(String, Int)]
tests2 = [(testInput, 1707)]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
