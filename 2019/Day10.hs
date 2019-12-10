module Main where

import Utilities
import Data.List
import Data.Ratio
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Point]

type Point = (Int, Int) -- (0,0) is top left

parse :: String -> Input
parse s =
    [(x, y) | (y, l) <- zip [0..] (lines s), (x, c) <- zip [0..] l, c == '#']

-- Part One

-- directions from pos that contain at least one point
view :: [Point] -> Point -> Set Point
view ps pos =
    Set.fromList [fst (normalize (p `minus` pos)) | p <- ps, p /= pos]

-- direction in reduced form, with common divisor
normalize :: Point -> (Point, Int)
normalize (x, y) = ((x `div` d, y `div` d), d)
  where
    d = gcd x y

-- difference between points
minus :: Point -> Point -> Point
minus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

-- point with the most other points in view, plus the count
best :: [Point] -> (Int, Point)
best ps = maximum [(Set.size (view ps p), p) | p <- ps]

solve1 :: Input -> Int
solve1 = fst . best

tests1 :: [(String, (Int, Point))]
tests1 = [
    (".#..#\n\
     \.....\n\
     \#####\n\
     \....#\n\
     \...##\n", (8, (3,4))),
    ("......#.#.\n\
     \#..#.#....\n\
     \..#######.\n\
     \.#.#.###..\n\
     \.#..#.....\n\
     \..#....#.#\n\
     \#..#....#.\n\
     \.##.#..###\n\
     \##...#..#.\n\
     \.#....####\n", (33, (5,8))),
    ("#.#...#.#.\n\
     \.###....#.\n\
     \.#....#...\n\
     \##.#.#.#.#\n\
     \....#.#.#.\n\
     \.##..###.#\n\
     \..#...##..\n\
     \..##....##\n\
     \......#...\n\
     \.####.###.\n", (35, (1,2))),
    (".#..#..###\n\
     \####.###.#\n\
     \....###.#.\n\
     \..###.##.#\n\
     \##.##.#.#.\n\
     \....###..#\n\
     \..#.#..#.#\n\
     \#..#.#.###\n\
     \.##...##.#\n\
     \.....#.#..\n", (41, (6,3))),
    (largeExample, (210, (11,13)))]

largeExample :: String
largeExample =
   ".#..##.###...#######\n\
   \##.############..##.\n\
   \.#.######.########.#\n\
   \.###.#######.####.#.\n\
   \#####.##.#.##.###.##\n\
   \..#####..#.#########\n\
   \####################\n\
   \#.####....###.#.#.##\n\
   \##.#################\n\
   \#####.##.###..####..\n\
   \..######..##.#######\n\
   \####.##.####...##..#\n\
   \.#####..#.######.###\n\
   \##...#.##########...\n\
   \#.##########.#######\n\
   \.####.#.###.###.#.##\n\
   \....##.##.###..#####\n\
   \.#.#.###########.###\n\
   \#.#.#.#####.####.###\n\
   \###.##.####.##.#..##\n"

-- Part Two

-- for each angle, a non-empty list of points at that angle, closest first
multiview :: [Point] -> Point -> Map Angle [Point]
multiview ps pos =
    fmap Map.elems $
    Map.fromListWith Map.union
        [(angle dp, Map.singleton (dist dp) p) |
            p <- ps, p /= pos, let dp = p `minus` pos]

-- a representation of angles increasing clockwise from straight up
data Angle = Angle {
    quadrant :: Int,   -- quadrant, counted clockwise from top right
    tan :: Ratio Int } -- tangent of angle from start of that quadrant
    deriving (Eq, Ord, Show)

-- angle of a non-zero point
angle :: Point -> Angle
angle (x, y)
  | x >= 0 && y < 0 = Angle 0 (- x % y)
  | y >= 0 && x > 0 = Angle 1 (y % x)
  | x <= 0 && y > 0 = Angle 2 (- x % y)
  | y <= 0 && x < 0 = Angle 3 (y % x)
  | otherwise = error "angle of (0,0)"

-- Manhattan distance
dist :: Point -> Int
dist (x, y) = abs x + abs y

viewFromBest :: [Point] -> Map Angle [Point]
viewFromBest ps = multiview ps (snd (best ps))

-- points in order encountered in clockwise sweeps
vaporize :: Map Angle [Point] -> [Point]
vaporize = concat . transpose . Map.elems

solve2 :: Input -> Int
solve2 ps = 100*x + y
  where
    (x, y) = vaporize (viewFromBest ps)!!199

tests2 :: [(String, Int)]
tests2 = [(largeExample, 802)]

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    putStr (unlines (failures "best" (best . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
