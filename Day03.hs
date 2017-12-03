-- square spiral
module Main where

import Utilities
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)

type Input = Int

type Coord = (Int, Int)

manhattan :: Coord -> Int
manhattan (x, y) = abs x + abs y

-- rotate one quadrant counterclockwise
rotate :: Coord -> Coord
rotate (x, y) = (y, -x)

-- position of cell n (counting from 1)
coord :: Int -> Coord
coord 1 = (0,0)
coord n = times side rotate (ring, ring - pos - 1)
  where
    ring = floor ((sqrt (fromIntegral (n-1)) + 1)/2)
    posOnRing = n-1 - (2*ring - 1)^2
    (side, pos) = divMod posOnRing (2*ring)

solve1 :: Input -> Int
solve1 n = manhattan (coord n)

tests1 :: [(Int, Int)]
tests1 = [(1, 0), (12, 3), (23, 2), (1024, 31)]

-- Part Two

coords :: [Coord]
coords = map coord [1..]

-- OEIS A141481
values :: [Int]
values = 1:snd (mapAccumL addNeighbours (Map.singleton (coord 1) 1) (tail coords))

addNeighbours :: Map Coord Int -> Coord -> (Map Coord Int, Int)
addNeighbours m (x, y) = (Map.insert (x, y) v m, v)
  where
    v = sum [Map.findWithDefault 0 n m | n <- neighbours]
    neighbours =
        [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

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
