module Main where

import Utilities
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified RationalList as Rep

-- Input processing

type Input = Landscape

type Landscape = [[Acre]]
data Acre = Open | Trees | Lumberyard
  deriving (Show, Eq, Ord)

parse :: String -> Input
parse = map (map acre) . lines
  where
    acre '.' = Open
    acre '|' = Trees
    acre '#' = Lumberyard
    acre c = error $ "unexpected character '" ++ [c] ++ "'"

showLandscape :: [[Acre]] -> String
showLandscape = unlines . map (map showAcre)
  where
    showAcre Open = '.'
    showAcre Trees = '|'
    showAcre Lumberyard = '#'

-- Part One

solve1 :: Input -> Int
solve1 = value . times 10 step

-- resource value = number of wooded acres * number of lumberyards
value :: Landscape -> Int
value land = count Trees as * count Lumberyard as
  where
    as = concat land
    count x xs = length (filter (== x) xs)

-- one step of the cellular automaton
step :: Landscape -> Landscape
step = map (map (uncurry rule . neighbourhood)) . neighbourhoods Open

-- new content depending on number of different adjacent types
rule :: Acre -> Map Acre Int -> Acre
rule Open neighbours
  | Map.findWithDefault 0 Trees neighbours >= 3 = Trees
rule Trees neighbours
  | Map.findWithDefault 0 Lumberyard neighbours >= 3 = Lumberyard
rule Lumberyard neighbours
  | Map.findWithDefault 0 Lumberyard neighbours == 0 = Open
rule Lumberyard neighbours
  | Map.findWithDefault 0 Trees neighbours == 0 = Open
rule t _ = t

type Triple a = (a, a, a)

-- central value and counts of neighbouring values
neighbourhood :: Ord a => Triple (Triple a) -> (a, Map a Int)
neighbourhood ((a, b, c), (d, e, f), (g, h, i)) =
    (e, Map.fromList (frequency [a, b, c, d, f, g, h, i]))

-- group neighbourhoods, with a padding value for the edges
neighbourhoods :: a -> [[a]] -> [[Triple (Triple a)]]
neighbourhoods pad =
    triplesWith (zipWith3 (,,)) (repeat (pad, pad, pad)) . map (triples pad)

triples :: a -> [a] -> [Triple a]
triples = triplesWith (,,)

-- group adjacent triples, with a padding value for the ends
triplesWith :: (a -> a -> a -> b) -> a -> [a] -> [b]
triplesWith f pad xs = zipWith3 f (pad:xs) xs (tail xs ++ [pad])

tests1 :: [(String, Int)]
tests1 = [(testInput, 1147)]

testInput :: String
testInput =
    ".#.#...|#.\n\
    \.....#|##|\n\
    \.|..|...#.\n\
    \..|#.....#\n\
    \#.#|||#|#|\n\
    \...#.||...\n\
    \.|....|...\n\
    \||...#|.#|\n\
    \|.||||..|.\n\
    \...#.|..|.\n"

-- Part Two

-- similar to solve1, but use RationalList to get the larger index
-- in a reasonable time
solve2 :: Input -> Int
solve2 = value . fromJust . Rep.elementAt 1000000000 . Rep.iterate step

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
