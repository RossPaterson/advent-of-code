module Main where

import Utilities
import Cartesian
import Data.List
import Data.Ratio
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Position] -- (0,0) is top left

parse :: String -> Input
parse s = [p | (p, c) <- readGrid s, c == '#']

-- Part One

-- directions from pos that contain at least one point
view :: [Position] -> Position -> Set Position
view ps pos =
    Set.fromList [fst (normalize (p .-. pos)) | p <- ps, p /= pos]

-- direction in reduced form, with common divisor
normalize :: Position -> (Position, Int)
normalize (Position x y) = (Position (x `div` d) (y `div` d), d)
  where
    d = gcd x y

-- point with the most other points in view, plus the count
best :: [Position] -> (Int, Position)
best ps = maximum [(Set.size (view ps p), p) | p <- ps]

solve1 :: Input -> Int
solve1 = fst . best

tests1 :: [(String, (Int, Position))]
tests1 = [
    (".#..#\n\
     \.....\n\
     \#####\n\
     \....#\n\
     \...##\n", (8, Position 3 4)),
    ("......#.#.\n\
     \#..#.#....\n\
     \..#######.\n\
     \.#.#.###..\n\
     \.#..#.....\n\
     \..#....#.#\n\
     \#..#....#.\n\
     \.##.#..###\n\
     \##...#..#.\n\
     \.#....####\n", (33, Position 5 8)),
    ("#.#...#.#.\n\
     \.###....#.\n\
     \.#....#...\n\
     \##.#.#.#.#\n\
     \....#.#.#.\n\
     \.##..###.#\n\
     \..#...##..\n\
     \..##....##\n\
     \......#...\n\
     \.####.###.\n", (35, Position 1 2)),
    (".#..#..###\n\
     \####.###.#\n\
     \....###.#.\n\
     \..###.##.#\n\
     \##.##.#.#.\n\
     \....###..#\n\
     \..#.#..#.#\n\
     \#..#.#.###\n\
     \.##...##.#\n\
     \.....#.#..\n", (41, Position 6 3)),
    (largeExample, (210, Position 11 13))]

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
multiview :: [Position] -> Position -> Map Angle [Position]
multiview ps pos =
    fmap Map.elems $
    Map.fromListWith Map.union
        [(angle dp, Map.singleton (norm dp) p) |
            p <- ps, p /= pos, let dp = p .-. pos]

-- a representation of angles increasing clockwise from straight up
data Angle = Angle {
    quadrant :: Int,   -- quadrant, counted clockwise from top right
    tan :: Ratio Int } -- tangent of angle from start of that quadrant
    deriving (Eq, Ord, Show)

-- angle of a non-zero point
angle :: Position -> Angle
angle (Position x y)
  | x >= 0 && y < 0 = Angle 0 (- x % y)
  | y >= 0 && x > 0 = Angle 1 (y % x)
  | x <= 0 && y > 0 = Angle 2 (- x % y)
  | y <= 0 && x < 0 = Angle 3 (y % x)
  | otherwise = error "angle of (0,0)"

viewFromBest :: [Position] -> Map Angle [Position]
viewFromBest ps = multiview ps (snd (best ps))

-- points in order encountered in clockwise sweeps
vaporize :: Map Angle [Position] -> [Position]
vaporize = concat . transpose . Map.elems

solve2 :: Input -> Int
solve2 ps = 100*x + y
  where
    Position x y = vaporize (viewFromBest ps)!!199

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
