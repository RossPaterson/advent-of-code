module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Wire, Wire)
type Wire = [Segment]

data Segment = Segment Direction Int
    deriving Show
data Direction = L | R | U | D
    deriving (Bounded, Enum, Show)

parse :: String -> Input
parse s = (w1, w2)
  where
    [w1, w2] = map (runParser wire) (lines s)
    wire = sepBy1 segment (char ',')
    segment = Segment <$> enumValue <*> nat

-- Part One

type Point = (Int, Int)

addPoints :: Point -> Point -> Point
addPoints (x1, y1) (x2, y2) = (x1+x2, y1+y2)

-- the points visited by a wire, in order
wirePoints :: Wire -> [Point]
wirePoints = tail . scanl addPoints (0, 0) . concatMap segmentDeltas

-- unit moves in a segment
segmentDeltas :: Segment -> [Point]
segmentDeltas (Segment d n) = replicate n (direction d)

direction :: Direction -> Point
direction U = (0, -1)
direction D = (0, 1)
direction L = (-1, 0)
direction R = (1, 0)

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

-- points visited by both wires
intersections :: Wire -> Wire -> [Point]
intersections w1 w2 = Set.elems $
    Set.intersection (Set.fromList (wirePoints w1))
        (Set.fromList (wirePoints w2))

solve1 :: Input -> Int
solve1 = minimum . map manhattan . uncurry intersections

testInput1 :: String
testInput1 = "\
\R8,U5,L5,D3\n\
\U7,R6,D4,L4\n\
\"

testInput2 :: String
testInput2 = "\
\R75,D30,R83,U83,L12,D49,R71,U7,L72\n\
\U62,R66,U55,R34,D71,R55,D58,R83\n\
\"

testInput3 :: String
testInput3 = "\
\R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\n\
\U98,R91,D20,R16,D67,R40,U7,R15,U6,R7\n\
\"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 6), (testInput2, 159), (testInput3, 135)]

-- Part Two

-- for each point visited by the wire, time of the first visit
firstVisits :: Wire -> Map Point Int
firstVisits w = Map.fromListWith (const id) (zip (wirePoints w) [1..])

-- for each point visited by both wires, the sum of the times of first visits
timedVisits :: Wire -> Wire -> Map Point Int
timedVisits w1 w2 = Map.intersectionWith (+) (firstVisits w1) (firstVisits w2)

solve2 :: Input -> Int
solve2 = minimum . Map.elems . uncurry timedVisits

tests2 :: [(String, Int)]
tests2 = [(testInput1, 30), (testInput2, 610), (testInput3, 410)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)