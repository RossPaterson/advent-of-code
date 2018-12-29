module Main where

import Components
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

solve1 :: Input -> Int
solve1 g = length (component (g!) 0)

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

solve2 :: Input -> Int
solve2 g = length (components (g!) (Map.keysSet g))

tests2 :: [(String, Int)]
tests2 = [(testInput, 2)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
