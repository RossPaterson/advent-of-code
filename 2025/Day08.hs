module Main where

import Geometry
import Parser
import Utilities
import Data.List (sortOn, tails)
import Data.Maybe (listToMaybe)
import Data.Ord (Down(Down))
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Point3]

parse :: String -> Input
parse = map (runParser point3) . lines
  where
    point3 = Point3 <$> nat <* char ',' <*> nat <* char ',' <*> nat

-- Part One

-- sets of connected elements
type Components a = [Set a]

initComponents :: [a] -> Components a
initComponents = map Set.singleton

solve1 :: Input -> Int
solve1 = measure . connect 1000

measure :: Components Point3 -> Int
measure = product . take 3 . sortOn Down . map Set.size

connect :: Int -> [Point3] -> Components Point3
connect n ps =
    foldl connectPair (initComponents ps) (take n (pairsByDistance ps))

-- Connect a pair of elements from the components, which might cause
-- two components to merge
connectPair :: Ord a => Components a -> (a, a) -> Components a
connectPair cs (x1, x2) = case pickBy (Set.member x1) cs of
    Nothing -> error "x1"
    Just (c1, cs')
      | Set.member x2 c1 -> cs
      | otherwise -> case pickBy (Set.member x2) cs' of
        Nothing -> error "x2"
        Just (c2, cs'') -> Set.union c1 c2:cs''

-- Extract the first element of the list satisfying the property, if any
pickBy :: (a -> Bool) -> [a] -> Maybe (a, [a])
pickBy p xs =
    listToMaybe [(x, front++back) | (front, x:back) <- splits xs, p x]

-- All pairs of distinct points, in order of increasing distance
pairsByDistance :: [Point3] -> [(Point3, Point3)]
pairsByDistance = sortOn (uncurry dist2) . unorderedPairs

-- All pairs of elements in the list such that the first occurs earlier
-- in the list than the second.
unorderedPairs :: [a] -> [(a, a)]
unorderedPairs xs = [(x1, x2) | x1:rest <- tails xs, x2 <- rest]

-- Square of Euclidean distance between the points
dist2 :: Point3 -> Point3 -> Int
dist2 (Point3 x1 y1 z1) (Point3 x2 y2 z2) =
    square (x1-x2) + square (y1-y2) + square (z1-z2)

square :: Int -> Int
square x = x*x

testInput :: String
testInput = "\
    \162,817,812\n\
    \57,618,57\n\
    \906,360,560\n\
    \592,479,940\n\
    \352,342,300\n\
    \466,668,158\n\
    \542,29,236\n\
    \431,825,988\n\
    \739,650,466\n\
    \52,470,668\n\
    \216,146,977\n\
    \819,987,18\n\
    \117,168,530\n\
    \805,96,715\n\
    \346,949,466\n\
    \970,615,88\n\
    \941,993,340\n\
    \862,61,35\n\
    \984,92,344\n\
    \425,690,689\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 40)]

-- Part Two

solve2 :: Input -> Int
solve2 ps = measure2 $ pairs !! (lastPair-1)
  where
    pairs = pairsByDistance ps
    lastPair = length $ takeWhile (> 1) $ map length $
        scanl connectPair (initComponents ps) pairs

measure2 :: (Point3, Point3) -> Int
measure2 (Point3 x1 _ _, Point3 x2 _ _) = x1*x2

tests2 :: [(String, Int)]
tests2 = [(testInput, 25272)]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (measure . connect 10 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
