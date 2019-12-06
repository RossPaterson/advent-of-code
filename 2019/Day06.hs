module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = Tree Body

-- tree represented as a mapping of each node to its parent
type Tree a = Map a a
type Body = String

parse :: String -> Input
parse = Map.fromList .map (runParser orbit) . lines
  where
    orbit = flip (,) <$> body <* char ')' <*> body
    body = some (digit <|> letter)

-- Part One

-- depth of each node
depths :: Ord a => Tree a -> Map a Int
depths t = depth_map
  where
    depth_map = fmap depth t
    depth a = maybe 1 (1+) (Map.lookup a depth_map)

solve1 :: Input -> Int
solve1 = sum . depths

testInput :: String
testInput = "\
\COM)B\n\
\B)C\n\
\C)D\n\
\D)E\n\
\E)F\n\
\B)G\n\
\G)H\n\
\D)I\n\
\E)J\n\
\J)K\n\
\K)L\n\
\"

tests1 :: [(String, Int)]
tests1 = [(testInput, 42)]

-- Part Two

-- path from each node to the root (not including the start node or the root)
paths :: Ord a => Tree a -> Map a [a]
paths t = path_map
  where
    path_map = fmap path t
    path a = maybe [] (a:) (Map.lookup a path_map)

-- sum of the lengths of the different parts of paths from two nodes
transfers :: Ord a => a -> a -> Map a [a] -> Int
transfers a b m =
    length ta + length tb - 2*length (filter id (zipWith (==) ta tb))
  where
    ta = reverse (m!a)
    tb = reverse (m!b)

solve2 :: Input -> Int
solve2 = transfers "YOU" "SAN" . paths

testInput2 :: String
testInput2 = "\
\COM)B\n\
\B)C\n\
\C)D\n\
\D)E\n\
\E)F\n\
\B)G\n\
\G)H\n\
\D)I\n\
\E)J\n\
\J)K\n\
\K)L\n\
\K)YOU\n\
\I)SAN\n\
\"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 4)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
