module Main where

import Parser
import Utilities
import Graph
import Data.Array

-- Input processing

type Input = (Int, Position)

parse :: String -> Input
parse s = (runParser depth ld, runParser target lt)
  where
    depth = string "depth: " *> nat
    target = (,) <$ string "target: " <*> nat <* char ',' <*> nat
    [ld, lt] = lines s

type Position = (Int, Int)

-- Part One

solve1 :: Input -> Int
solve1 (depth, (xt, yt)) =
    sum [fromEnum r | r <- elems (makeCave (xt+1) (yt+1) depth (xt, yt))]

type Cave = Array Position Region
data Region = Rocky | Wet | Narrow
  deriving (Show, Enum)

-- build a cave according to the rules in the puzzle
makeCave :: Int -> Int -> Int -> Position -> Cave
makeCave w h depth (xt, yt) = fmap regionType erosionLevel
  where
    erosionLevel =
        array ((0,0), (w-1,h-1))
            [((x, y), erosion (x, y)) | x <- [0..w-1], y <- [0..h-1]]
    erosion p = (geologicIndex p + depth) `mod` modulus
    geologicIndex (x, y)
      | x == xt && y == yt = 0
      | y == 0 = x * xincr
      | x == 0 = y * yincr
      | otherwise = erosionLevel!(x-1, y) * erosionLevel!(x, y-1)

xincr, yincr :: Int
xincr = 16807
yincr = 48271

modulus :: Int
modulus = 20183

regionType :: Int -> Region
regionType e = toEnum (e `mod` 3)

-- string representation of a cave
showCave :: Position -> Cave -> String
showCave (xt, yt) cave =
    unlines [[showPoint (x, y) | x <- [0..xb]] | y <- [0..yb]]
  where
    showPoint (x, y)
      | x == 0 && y == 0 = 'M'
      | x == xt && y == yt = 'T'
      | otherwise = showRegion (cave!(x, y))
    (xb, yb) = snd (bounds cave)

showRegion :: Region -> Char
showRegion Rocky = '.'
showRegion Wet = '='
showRegion Narrow = '|'

tests1 :: [(Input, Int)]
tests1 = [((510, (10,10)), 114)]

-- Part Two

-- minimum number of minutes from the start to the finish
-- We don't know how big the search area should be, so run the search
-- twice: once to get an upper bound, and again to get the optimum.
solve2 :: Input -> Int
solve2 (depth, target) = constrainedFastest margin depth target
  where
    (xt, yt) = target
    -- There is always a Manhattan path to the target, even if we need
    -- to change equipment on each move and again to use the torch when
    -- we reach the target, but there may also be faster paths that
    -- stay within that rectangle.
    -- That gives us an upper bound on the length of an optimal path.
    bound = constrainedFastest 0 depth target
    -- furthest out a faster route could go and come back to the target
    margin = (bound - (xt+yt)) `div` 2

-- minimum number of minutes from the start to the finish on routes that
-- do not go more than margin past either of the target's coordinates
constrainedFastest :: Int -> Int -> Position -> Int
constrainedFastest margin depth target = length $
    takeWhile (finalState target `notElem`) $
        bfs (moves cave) [startState]
  where
    (xt, yt) = target
    cave = makeCave (xt+margin+1) (yt+margin+1) depth target

data State = State {
    pos :: Position, -- current position in the cave
    equipment :: Equipment, -- equipment we have or are switching to
    waitTime :: Int } -- time until our equipment is ready for use
  deriving (Show, Eq, Ord)
data Equipment = Torch | ClimbingGear | Neither
  deriving (Show, Eq, Ord, Bounded, Enum)

-- start at the origin with the torch
startState :: State
startState = State (0, 0) Torch 0

-- finish at the target with the torch ready for use
finalState :: Position -> State
finalState target = State target Torch 0

-- things we can do in a minute from a given state
moves :: Cave -> State -> [State]
moves cave (State p e t)
  | t == 0 =
    -- move to neighbouring region with current equipment
    [State p' e 0 | p' <- neighbours cave p, works e (cave!p')] ++
    -- start switching to different equipment, which will take 7 minutes
    [State p e' 6 | e' <- allValues, e' /= e, works e' (cave!p)]
    -- continue switching to different equipment
  | otherwise = [State p e (t-1)]

-- neighbouring positions in the cave
neighbours :: Cave -> Position -> [Position]
neighbours cave (x, y) =
    filter (inRange (bounds cave)) [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]

-- Does this equipment work in this region type?
works :: Equipment -> Region -> Bool
works Neither Rocky = False
works Torch Wet = False
works ClimbingGear Narrow = False
works _ _ = True

tests2 :: [(Input, Int)]
tests2 = [((510, (10,10)), 45)]

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 input)
