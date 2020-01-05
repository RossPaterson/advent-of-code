-- square spiral
module Main where

import Utilities
import Cartesian
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

type Input = Int

-- rotate one quadrant counterclockwise
rotate :: Point2 -> Point2
rotate (Point2 x y) = Point2 y (-x)

-- position of cell n (counting from 1)
coord :: Int -> Point2
coord 1 = zero
coord n = times side rotate (Point2 ring (ring - pos - 1))
  where
    -- spiral inside ring r contains (2*r - 1)^2 elements
    ring = floor ((sqrt (fromIntegral (n-1)) + 1)/2)
    posOnRing = n-1 - (2*ring - 1)^(2::Int)

    -- each of the 4 sides of the ring r has 2*r elements
    (side, pos) = divMod posOnRing (2*ring)

-- OEIS A214526
solve1 :: Input -> Int
solve1 n = norm (coord n)

tests1 :: [(Int, Int)]
tests1 = [(1, 0), (12, 3), (23, 2), (1024, 31)]

-- Part Two

coords :: [Point2]
coords = map coord [1..]

-- alternative iterative definition
coords' :: [Point2]
coords' = iterate next zero
  where
    next :: Point2 -> Point2
    next (Point2 x y)
      | x > abs y = Point2 x (y-1)
      | y < - abs x || y < 0 && y == - x = Point2 (x-1) y
      | x < - abs y || x < 0 && x == y = Point2 x (y+1)
      | y >= abs x = Point2 (x+1) y

-- OEIS A141481
values :: [Int]
values = 1:snd (mapAccumL addNeighbours (Map.singleton (coord 1) 1) (tail coords))

addNeighbours :: Map Point2 Int -> Point2 -> (Map Point2 Int, Int)
addNeighbours m p = (Map.insert p v m, v)
  where
    v = sum [Map.findWithDefault 0 (p .+. n) m | n <- neighbourhood]

-- points adjacent to (0,0)
neighbourhood :: [Point2]
neighbourhood =
    [Point2 dx dy | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

test2 :: Bool
test2 = [1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147, 304, 330, 351, 362, 747, 806] `isPrefixOf` values

solve2 :: Input -> Int
solve2 n = head $ dropWhile (<= n) values

input :: Int
input = 325489

main :: IO ()
main = do
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    print (solve2 input)
