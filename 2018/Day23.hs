module Main where

import Cartesian
import Parser
import qualified MaxPriorityQueue as PQ
import Utilities
import Data.Foldable
import Data.Ord

-- Input processing

type Input = System

type System = [Region]

-- The set of points whose Manhattan distance from the position of a
-- nanobot is no more than the radius is a regular octahedron with its
-- vertices in the axis directions.
data Region = Octahedron { centre :: Point3, radius :: Int }
  deriving Show

parse :: String -> Input
parse = map (runParser octahedron) . lines
  where
    octahedron = Octahedron <$ string "pos=" <*> point <*
        string ", r=" <*> nat
    point = Point3 <$ char '<' <*> int <* char ',' <*> int <*
        char ',' <*> int <* char '>'

-- Part One

-- number of centres within range of the largest octahedron
solve1 :: Input -> Int
solve1 ns = length [n | n <- ns, inside (centre n) largest]
  where
    largest = maximumBy (comparing radius) ns

-- is the point inside the region?
inside :: Point3 -> Region -> Bool
inside p (Octahedron c r) = distance p c <= r

tests1 :: [(String, Int)]
tests1 = [(testInput1, 7)]

testInput1 :: String
testInput1 =
    "pos=<0,0,0>, r=4\n\
    \pos=<1,0,0>, r=1\n\
    \pos=<4,0,0>, r=3\n\
    \pos=<0,2,0>, r=1\n\
    \pos=<0,5,0>, r=3\n\
    \pos=<0,0,3>, r=1\n\
    \pos=<1,1,1>, r=1\n\
    \pos=<1,1,2>, r=1\n\
    \pos=<1,3,1>, r=1\n"

-- Part Two

-- shortest distance from the origin of points that are within the
-- most regions.
-- Uses a directed search of cubes of candidate points, starting with a
-- cube containing all the regions, and at each stage examining the
-- code that intersects with the most regions and dividing it into
-- eight subcubes for further consideration.
solve2 :: [Region] -> Int
solve2 ns = searchCubes (newCube (boundingCube ns))
  where
    -- The priority ordering ensures that if the least is a cube of size 1,
    -- it is a point inside the most regions that is closest to the origin.
    searchCubes pq = case PQ.extract pq of
        Nothing -> error "empty queue"
        Just (Priority _ s (Down dist), c, pq')
          | s == 1 -> dist
          | otherwise -> searchCubes (pq' <> foldMap newCube (splitCube c))

    -- priority queue containing a single cube
    newCube c = PQ.singleton (priority ns c) c

-- cubic search area, with minimal corner and size (a power of 2)
data Cube = Cube Point3 Int
  deriving Show

-- cube containing all the regions
boundingCube :: [Region] -> Cube
boundingCube ps = Cube (Point3 xmin ymin zmin) size
  where
    xmin = minimum [x-r | Octahedron (Point3 x _ _) r <- ps]
    xmax = maximum [x+r | Octahedron (Point3 x _ _) r <- ps]
    ymin = minimum [y-r | Octahedron (Point3 _ y _) r <- ps]
    ymax = maximum [y+r | Octahedron (Point3 _ y _) r <- ps]
    zmin = minimum [z-r | Octahedron (Point3 _ _ z) r <- ps]
    zmax = maximum [z+r | Octahedron (Point3 _ _ z) r <- ps]
    maxdim = ((xmax - xmin) `max` (ymax - ymin) `max` (zmax - zmin)) + 1
    -- smallest power of two >= maxdim
    size = head $ dropWhile (< maxdim) $ iterate (*2) 1

-- split the cube (with s a power of 2) into 8 smaller cubes
splitCube :: Cube -> [Cube]
splitCube (Cube p s) =
    [Cube (p .+. Point3 dx dy dz) s2 |
        dx <- [0, s2], dy <- [0, s2], dz <- [0, s2]]
  where
    s2 = s `div` 2

-- A search cube has greater priority if it intersects with a larger
-- number of regions, then if it has a larger side, then smaller distance.
-- This ordering has the properties:
-- * The priority for a search cube is >= that for any included cube.
-- * If the highest priority cube has size 1, it is the solution.
data Priority = Priority Int Int (Down Int)
  deriving (Show, Eq, Ord)

-- priority for a search cube
priority :: [Region] -> Cube -> Priority
priority ns c@(Cube p s) =
    Priority (numIntersects c ns) s (Down (norm p))

-- number of regions containing any point in the cube
numIntersects :: Cube -> [Region] -> Int
numIntersects c = length . filter (intersectCube c)

-- does the region share any points with the cube?
intersectCube :: Cube -> Region -> Bool
intersectCube (Cube (Point3 x y z) s) (Octahedron (Point3 cx cy cz) r) =
    dx + dy + dz <= r
  where
    dx = 0 `max` (x - cx) `max` (cx - (x + s - 1))
    dy = 0 `max` (y - cy) `max` (cy - (y + s - 1))
    dz = 0 `max` (z - cz) `max` (cz - (z + s - 1))

tests2 :: [(String, Int)]
tests2 = [(testInput2, 36)]

testInput2 :: String
testInput2 =
    "pos=<10,12,12>, r=2\n\
    \pos=<12,14,12>, r=2\n\
    \pos=<16,12,12>, r=4\n\
    \pos=<14,14,14>, r=6\n\
    \pos=<50,50,50>, r=200\n\
    \pos=<10,10,10>, r=5\n"

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
