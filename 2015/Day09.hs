module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe

type Node = String
type Input = Map (Node, Node) Int

parse :: String -> Input
parse = Map.fromList . map (runParser dist) . lines
  where
    dist = (,) <$> edge <* string " = " <*> nat
    edge = (,) <$> node <* string " to " <*> node
    node = some letter

getDist :: Input -> Node -> Node -> Int
getDist m src dest =
    fromMaybe maxBound (Map.lookup (src, dest) m <|> Map.lookup (dest, src) m)

nodes :: Input -> [Node]
nodes m = fast_nub $ concat [[src, dest] | (src, dest) <- Map.keys m]

solve1 :: Input -> Int
solve1 m =
    minimum [sum (zipWith (getDist m) ns (tail ns)) |
        ns <- permutations (nodes m)]

-- Part Two --

solve2 :: Input -> Int
solve2 m =
    maximum [sum (zipWith (getDist m) ns (tail ns)) |
        ns <- permutations (nodes m)]

main :: IO ()
main = do
    s <- readFile "input09.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
