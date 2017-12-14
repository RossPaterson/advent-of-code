module Main where

import Components
import Knothash
import Utilities
import Data.Bits
import Data.Set (Set)
import qualified Data.Set as Set

type Position = (Int, Int)
type Grid = Set Position

type Input = Grid

parse :: String -> Input
parse = mkGrid

mkGrid :: String -> Grid
mkGrid key = Set.fromList [(r, pos*8 + bit) |
    r <- [0..127],
    (pos, h) <- zip [0..] (knothash (key ++ "-" ++ show r)),
    bit <- [0..7],
    testBit h (7-bit)]

showGrid :: Grid -> String
showGrid g =
    unlines [[if Set.member (r, c) g then '#' else '.' | c <- [0..127]] |
        r <- [0..127]]

solve1 :: Input -> Int
solve1 = Set.size

testInput :: Input
testInput = mkGrid "flqrgnkx"

input :: Input
input = mkGrid "hfdlxzhv"

tests1 :: [(Input, Int)]
tests1 = [(testInput, 8108)]

-- Part Two

neighbours :: Grid -> Position -> [Position]
neighbours g (pos@(x,y))
  | Set.member pos g =
    [pos' | pos' <- [(x-1,y), (x,y-1), (x+1,y), (x,y+1)], Set.member pos' g]
  | otherwise = []

regions :: Grid -> [Set Position]
regions g = components (neighbours g) g

solve2 :: Input -> Int
solve2 = length . regions

tests2 :: [(Input, Int)]
tests2 = [(testInput, 1242)]

main :: IO ()
main = do
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 input)
