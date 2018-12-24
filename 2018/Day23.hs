module Main where

import Parser
import PriorityQueue (PQ)
import qualified PriorityQueue as PQ
import Utilities
import Data.List
import Data.Ord

-- Input processing

type Input = System

type System = [Nanobot]
data Nanobot = Nanobot { pos :: Position, radius :: Int }
  deriving Show
type Position = (Int, Int, Int)

parse :: String -> Input
parse = map (runParser nanobot) . lines
  where
    nanobot = Nanobot <$ string "pos=" <*> point <*
        string ", r=" <*> nat
    point = (,,) <$ char '<' <*> int <* char ',' <*> int <*
        char ',' <*> int <* char '>'

-- Part One

-- number of nanobots within range of the one with the largest radius
solve1 :: Input -> Int
solve1 ns = length [n | n <- ns, distance (pos n) p <= r]
  where
    Nanobot p r = maximumBy (comparing radius) ns

-- Manhattan distance
distance :: Position -> Position -> Int
distance (x1, y1, z1) (x2, y2, z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

tests1 :: [(String, Int)]
tests1 = [(testInput1, 7)]

testInput1 = "\
\pos=<0,0,0>, r=4\n\
\pos=<1,0,0>, r=1\n\
\pos=<4,0,0>, r=3\n\
\pos=<0,2,0>, r=1\n\
\pos=<0,5,0>, r=3\n\
\pos=<0,0,3>, r=1\n\
\pos=<1,1,1>, r=1\n\
\pos=<1,1,2>, r=1\n\
\pos=<1,3,1>, r=1\n"

-- Part Two

-- shortest distance from the origin of points that are within range of
-- the most nanobots.
-- Uses a directed search of cubes of candidate points, starting with a
-- cube containing all the nanobot ranges, and at each stage examining
-- the code that intersects with the most nanobot ranges and dividing
-- it into eight subcubes for further consideration.
solve2 :: [Nanobot] -> Int
solve2 ns = searchCubes (add (boundingCube ns) PQ.empty)
  where
    -- The ordering on keys ensures that if the least is a cube of size 1,
    -- it is a point inside the most nanobot ranges that has the least
    -- distance to the origin.
    searchCubes pq = case PQ.extract pq of
        Nothing -> error "empty queue"
        Just (k@(Key _ (Down dist) s), c, pq')
          | s == 1 -> dist
          | otherwise -> searchCubes (foldr add pq' (splitCube c))

    -- add a cube to the priority queue in appropriate order
    add c pq = PQ.insert (makeKey ns c) c pq

-- cubic region, with bottom corner and size (a power of 2)
data Cube = Cube Position Int
  deriving Show

-- cube containing all the ranges of the nanobots
boundingCube :: [Nanobot] -> Cube
boundingCube ps = Cube (xmin, ymin, zmin) size
  where
    xmin = minimum [x-r | Nanobot (x, y, z) r <- ps]
    xmax = maximum [x+r | Nanobot (x, y, z) r <- ps]
    ymin = minimum [y-r | Nanobot (x, y, z) r <- ps]
    ymax = maximum [y+r | Nanobot (x, y, z) r <- ps]
    zmin = minimum [z-r | Nanobot (x, y, z) r <- ps]
    zmax = maximum [z+r | Nanobot (x, y, z) r <- ps]
    maxdim = ((xmax - xmin) `max` (ymax - ymin) `max` (xmax - zmin)) + 1
    size = head $ dropWhile (< maxdim) $ iterate (*2) 1

-- split the cube into 8 smaller cubes
splitCube :: Cube -> [Cube]
splitCube (Cube (x, y, z) s) =
    [Cube (x + dx, y + dy, z + dz) s2 |
        dx <- [0, s2], dy <- [0, s2], dz <- [0, s2]]
  where
    s2 = s `div` 2

-- maximum number of nanobots within range of any point in the cube
numIntersects :: [Nanobot] -> Cube -> Int
numIntersects ns c = length [n | n <- ns, intersectCube n c]

-- does the nanobot's range share any points with the cube?
intersectCube :: Nanobot -> Cube -> Bool
intersectCube (Nanobot (cx, cy, cz) r) (Cube (x, y, z) s) = dx + dy + dz <= r
  where
    dx = 0 `max` (x - cx) `max` (cx - (x + s - 1))
    dy = 0 `max` (y - cy) `max` (cy - (y + s - 1))
    dz = 0 `max` (z - cz) `max` (cz - (z + s - 1))

-- smaller (earlier in the priority queue) if larger number of intersections,
-- then larger cube, then distance.
-- Then if the smallest has size 1, it is the solution.
data Key = Key (Down Int) (Down Int) Int
  deriving (Show, Eq, Ord)

-- priority key for a search cube
makeKey :: [Nanobot] -> Cube -> Key
makeKey ns c@(Cube p s) =
    Key (Down (numIntersects ns c)) (Down (distance p (0,0,0))) s

tests2 :: [(String, Int)]
tests2 = [(testInput2, 36)]

testInput2 = "\
\pos=<10,12,12>, r=2\n\
\pos=<12,14,12>, r=2\n\
\pos=<16,12,12>, r=4\n\
\pos=<14,14,14>, r=6\n\
\pos=<50,50,50>, r=200\n\
\pos=<10,10,10>, r=5\n"

main :: IO ()
main = do
    s <- readFile "input23.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
