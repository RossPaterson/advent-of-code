-- square spiral
module Main where

import Utilities
import Geometry
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

type Input = Int

-- rotate one quadrant counterclockwise
rotate :: Position -> Position
rotate (Position x y) = Position y (-x)

-- position of cell n (counting from 1)
coord :: Int -> Position
coord 1 = zero
coord n = times side rotate (Position ring (ring - pos - 1))
  where
    -- spiral inside ring r contains (2*r - 1)^2 elements
    ring = floor ((sqrt (fromIntegral (n-1)::Double) + 1)/2)
    posOnRing = n-1 - (2*ring - 1)^(2::Int)

    -- each of the 4 sides of the ring r has 2*r elements
    (side, pos) = divMod posOnRing (2*ring)

-- OEIS A214526
solve1 :: Input -> Int
solve1 n = norm (coord n)

tests1 :: [(Int, Int)]
tests1 = [(1, 0), (12, 3), (23, 2), (1024, 31)]

-- Part Two

coords :: [Position]
coords = map coord [1..]

-- alternative iterative definition
coords' :: [Position]
coords' = iterate next zero
  where
    next :: Position -> Position
    next (Position x y)
      | x > abs y = Position x (y-1)
      | y < - abs x || y < 0 && y == - x = Position (x-1) y
      | x < - abs y || x < 0 && x == y = Position x (y+1)
      | y >= abs x = Position (x+1) y
      | otherwise = error "uncovered case"

-- OEIS A141481
values :: [Int]
values =
    1:snd (mapAccumL addNeighbours (Map.singleton (coord 1) 1) (tail coords))

addNeighbours :: Map Position Int -> Position -> (Map Position Int, Int)
addNeighbours m p = (Map.insert p v m, v)
  where
    v = sum [Map.findWithDefault 0 (p .+. n) m | n <- neighbourhood]

-- points adjacent to (0,0)
neighbourhood :: [Position]
neighbourhood =
    [Position dx dy | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

test2 :: Bool
test2 = [1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147, 304, 330, 351, 362, 747, 806] `isPrefixOf` values

solve2 :: Input -> Int
solve2 n = head $ dropWhile (<= n) values

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = read s
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    print (solve2 input)
