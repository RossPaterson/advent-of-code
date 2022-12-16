module Main where

import Utilities
import Graph
import Parser
import Control.Applicative
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

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

-- Rose trees

data Tree a = Node a (Forest a)
    deriving (Show)
type Forest a = [Tree a]

instance Functor Tree where
    fmap f (Node x ts) = Node (f x) (map (fmap f) ts)

showTree :: (Show a) => Tree a -> String
showTree t = showsTree 0 t ""
  where
    showsTree n (Node x ts) =
        showString (replicate (2*n) ' ') . shows x . showChar '\n' .
        flip (foldr id) (map (showsTree (n+1)) ts)

iterateTree :: (a -> [a]) -> a -> Tree a
iterateTree f x = Node x (map (iterateTree f) (f x))

-- downwards accumulation
accumTree :: (b -> a -> b) -> b -> Tree a -> Tree b
accumTree f s (Node x ts) = Node s' (map (accumTree f s') ts)
  where
    s' = f s x

maxTree :: (Ord a) => Tree a -> a
maxTree (Node x ts) = maximum (x:map maxTree ts)

-- Special valves

start :: ValveName
start = "AA"

workingValves :: Valves -> [ValveName]
workingValves vs = [n | (n, v) <- Map.assocs vs, flow_rate v > 0]

workingValve :: Valves -> ValveName -> Bool
workingValve vs n = flow_rate (vs ! n) > 0

-- Weighted graph of working valves

type WeightedGraph a = Map a (Map a Int)

-- edges from the start and working valves to other working valves
-- The edge cost is the number of moves plus one to turn the valve on.
valveGraph :: Valves -> WeightedGraph ValveName
valveGraph vs =
    Map.fromList [(source, out_edges source) | source <- sources]
  where
    sources = start : workingValves vs
    out_edges source = Map.fromList [(target, dist) |
        (dist, targets) <- tail $ zip [1..] $ bfs (exits . (vs !)) [source],
        target <- targets, workingValve vs target]

-- Visit a list of working valves in all possible orders

data State = State {
    location :: ValveName,
    time_left :: Int,
    valves_left :: [ValveName]
    }
    deriving (Show)

explore :: WeightedGraph ValveName -> State -> [State]
explore g s =
    [State {
        location = target,
        time_left = time_left s - dist,
        valves_left = front++back
        } |
        (front, target:back) <- splits (valves_left s),
        dist <- maybeToList (Map.lookup target exit_map),
        dist < time_left s]
  where
    exit_map = g ! location s

valve_flow :: Valves -> State -> Int
valve_flow vs s = time_left s * flow_rate (vs ! location s)

max_flow :: Valves -> WeightedGraph ValveName -> Int -> [ValveName] -> Int
max_flow vs g time ns =
    maxTree $ accumTree (+) 0 $ fmap (valve_flow vs) $
        iterateTree (explore g) s0
  where
    s0 = State { location = start, time_left = time, valves_left = ns }

solve1 :: Input -> Int
solve1 vs = max_flow vs (valveGraph vs) 30 (workingValves vs)

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

-- ways of sharing a list
shares :: [a] -> [([a], [a])]
shares [] = [([], [])]
shares (x:xs) = [p | (ys, zs) <- shares xs, p <- [(x:ys, zs), (ys, x:zs)]]

-- same, except always put the first element in the first list
shares' :: [a] -> [([a], [a])]
shares' [] = [([], [])]
shares' (x:xs) = [(x:ys, zs) | (ys, zs) <- shares xs]

solve2 :: Input -> Int
solve2 vs =
    maximum [max_flow vs g 26 xs + max_flow vs g 26 ys |
        (xs, ys) <- shares' (workingValves vs)]
  where
    g = valveGraph vs

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
