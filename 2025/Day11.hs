module Main where

import Graph
import Parser
import Utilities
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

type Input = FiniteGraph Device
type Device = String

parse :: String -> Input
parse s = relation
    [(st, e) | (st, es) <- map (runParser connection) (lines s), e <- es]
  where
    connection = (,) <$> device <* string ": " <*> devices
    devices = device `sepBy1` char ' '
    device = some letter

-- Part One

solve1 :: Input -> Int
solve1 g = numPaths g "you" "out"

-- number of paths in the graph from start to finish, assuming that the
-- graph is acyclic
numPaths :: Ord a => FiniteGraph a -> a -> a -> Int
numPaths g start finish =
    Map.findWithDefault 0 start (numPathsMap g finish)

-- memoized numPaths
numPathsMap :: Ord a => FiniteGraph a -> a -> Map a Int
numPathsMap g target = m
  where
    m = Map.insert target 1 (Map.map num_paths g)
    num_paths ns = sum [Map.findWithDefault 0 n m | n <- Set.elems ns]

testInput :: String
testInput = "\
    \aaa: you hhh\n\
    \you: bbb ccc\n\
    \bbb: ddd eee\n\
    \ccc: ddd eee fff\n\
    \ddd: ggg\n\
    \eee: out\n\
    \fff: out\n\
    \ggg: out\n\
    \hhh: ccc fff iii\n\
    \iii: out\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 5)]

-- Part Two

solve2 :: Input -> Int
solve2 g =
    numPathsThrough g ["svr", "fft", "dac", "out"] +
    numPathsThrough g ["svr", "dac", "fft", "out"]

-- number of paths passing through the listed nodes in order
numPathsThrough :: Ord a => FiniteGraph a -> [a] -> Int
numPathsThrough _ [] = error "empty list of paths"
numPathsThrough g (n:ns) = product (zipWith (numPaths g) (n:ns) ns)

testInput2 :: String
testInput2 = "\
    \svr: aaa bbb\n\
    \aaa: fft\n\
    \fft: ccc\n\
    \bbb: tty\n\
    \tty: ccc\n\
    \ccc: ddd eee\n\
    \ddd: hub\n\
    \hub: fff\n\
    \eee: dac\n\
    \dac: fff\n\
    \fff: ggg hhh\n\
    \ggg: out\n\
    \hhh: out\n\
    \"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 2)]

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
