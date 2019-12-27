module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
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
nodes m = fastNub $ concat [[src, dest] | (src, dest) <- Map.keys m]

solve1 :: Input -> Int
solve1 m =
    minimum [sum (zipWith (getDist m) ns (tail ns)) |
        ns <- permutations (nodes m)]

testInput :: String
testInput =
    "London to Dublin = 464\n\
    \London to Belfast = 518\n\
    \Dublin to Belfast = 141\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 605)]

-- Part Two --

solve2 :: Input -> Int
solve2 m =
    maximum [sum (zipWith (getDist m) ns (tail ns)) |
        ns <- permutations (nodes m)]

tests2 :: [(String, Int)]
tests2 = [(testInput, 982)]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
