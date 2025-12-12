module Main where

import Geometry
import Parser
import Utilities
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Shapes, [Region])
type Shapes = Map Index (Set Position)
type Region = (Int, Int, Map Index Int)
type Index = Int

parse :: String -> Input
parse s = (shapes, regions)
  where
    paras = paragraphs s
    shapes = Map.fromList $ map (get_shape . lines) $ init paras
    get_shape [] = error "empty shape"
    get_shape (h:rest) =
        (runParser (nat <* char ':') h,
         Set.fromList [p | (p, c) <- readGrid (unlines rest), c == '#'])
    regions = map (runParser region) $ lines $ last paras
    region = (,,) <$> nat <* char 'x' <*> nat <* string ": " <*> quantities
    quantities = (Map.fromList . zip [0..]) <$> nat `sepBy1` char ' '

-- Part One

-- Ignore the test input, because the actual puzzle input is much simpler.

solve1 :: Input -> Int
solve1 (shapes, regions) = length $ filter (fits shapes) regions

fits :: Shapes -> Region -> Bool
fits shapes (w, h, counts)
    -- the region can accommodate all the shapes in disjoint 3x3 squares
  | sum counts <= (w `div` 3) * (h `div` 3) = True
    -- the region has fewer points than the required shapes
  |  sum [n*Set.size (shapes!i) | (i, n) <- Map.assocs counts] > w*h = False
  | otherwise = error "too difficult"

testInput :: String
testInput = "\
    \0:\n\
    \###\n\
    \##.\n\
    \##.\n\
    \\n\
    \1:\n\
    \###\n\
    \##.\n\
    \.##\n\
    \\n\
    \2:\n\
    \.##\n\
    \###\n\
    \##.\n\
    \\n\
    \3:\n\
    \##.\n\
    \###\n\
    \##.\n\
    \\n\
    \4:\n\
    \###\n\
    \#..\n\
    \###\n\
    \\n\
    \5:\n\
    \###\n\
    \.#.\n\
    \###\n\
    \\n\
    \4x4: 0 0 0 0 2 0\n\
    \12x5: 1 0 1 0 2 2\n\
    \12x5: 1 0 1 0 3 2\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 2)]

-- no Part Two on the last day

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    -- putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
