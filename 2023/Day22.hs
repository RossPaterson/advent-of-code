module Main where

import Geometry
import Parser
import Utilities
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Brick = (Point3, Point3)

type Input = [Brick]

parse :: String -> Input
parse = map (runParser brick) . lines
  where
    brick = (,) <$> point <* char '~' <*> point
    point = Point3 <$> nat <* char ',' <*> nat <* char ',' <*> nat

-- Part One

-- properties of a brick

top :: Brick -> Int
top (Point3 _ _ _, Point3 _ _ z2) = z2

bottom :: Brick -> Int
bottom (Point3 _ _ z1, Point3 _ _ _) = z1

-- horizontal cross-section of the brick
footprint :: Brick -> AABox Point2
footprint (Point3 x1 y1 _, Point3 x2 y2 _) =
    boundingBox [Point2 x1 y1, Point2 x2 y2]

-- (1) Drop each brick, from lowest to highest, until it comes to rest,
-- yielding a stack of bricks.

-- a pile of bricks, each of which is supported by the floor or
-- another brick
type Stack = [Brick]

-- Drop bricks from lowest to highest, until each comes to rest.
drop_all :: [Brick] -> Stack
drop_all bs = foldl drop_brick [] (sortOn bottom bs)

-- Drop a brick onto a stack of all the bricks that started up lower
-- than this one.
drop_brick :: Stack -> Brick -> Stack
drop_brick bs b = translate_brick (Point3 0 0 (-d)) b:bs
  where
    d = bottom b - h - 1
    h = support_height (footprint b) bs

-- Move a brick by p.
translate_brick :: Point3 -> Brick -> Brick
translate_brick p (p1, p2) = (p1 .+. p, p2 .+. p)

-- The highest point in the stack within the footprint of our brick
support_height :: AABox Point2 -> Stack -> Int
support_height box bs =
    maximum (0:[top b | b <- bs, isJust (intersectBox box (footprint b))])

-- (2) For a stack of bricks, work out which bricks are holding up
-- each brick.

-- bricks and the set of bricks each is resting on
-- Bricks on the floor are omitted.
support_map :: Stack -> Map Brick (Set Brick)
support_map bs =
    Map.fromListWith Set.union
        [(b1, Set.singleton b2) | (b1, b2) <- resting bs]

-- pairs of bricks such that the first is resting on the second
resting :: Stack -> [(Brick, Brick)]
resting bs =
    concat $ Map.elems $ Map.intersectionWith overlaps bottoms tops
  where
    tops = Map.fromListWith (++) [(top b, [b]) | b <- bs]
    bottoms = Map.fromListWith (++) [(bottom b-1, [b]) | b <- bs]

-- pairs of bricks from the respective lists whose footprints overlap
overlaps :: [Brick] -> [Brick] -> [(Brick, Brick)]
overlaps bs1 bs2 =
    [(b1, b2) |
        (b1, box1) <- bbs1,
        (b2, box2) <- bbs2,
        isJust (intersectBox box1 box2)]
  where
    withFootprint b = (b, footprint b)
    bbs1 = map withFootprint bs1
    bbs2 = map withFootprint bs2

-- bricks holding b up
support :: Map Brick (Set Brick) -> Brick -> [Brick]
support sm b = Set.elems (Map.findWithDefault Set.empty b sm)

-- (3) A brick is safe to remove if it is not the sole support of
-- another brick.
safe :: Stack -> [Brick]
safe stack =
    [b | b <- stack, null [b' | b' <- stack, support sm b' == [b]]]
  where
    sm = support_map stack

solve1 :: Input -> Int
solve1 = length . safe . drop_all

testInput :: String
testInput = "\
    \1,0,1~1,2,1\n\
    \0,0,2~2,0,2\n\
    \0,2,3~2,2,3\n\
    \0,0,4~0,2,4\n\
    \2,0,5~2,2,5\n\
    \0,1,6~2,1,6\n\
    \1,1,8~1,1,9\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 5)]

-- Part Two

-- Add b to gone if all supports of b are gone, so that b would fall.
-- Bricks are processed in ascending order of the heights of their bases.
fall :: Map Brick (Set Brick) -> [Brick] -> Brick -> [Brick]
fall sm gone b
  | bottom b > 1 && and [elem b' gone | b' <- support sm b] = b:gone
  | otherwise = gone

-- number that would fall if b disappeared
num_falling :: Map Brick (Set Brick) -> Stack -> Brick -> Int
num_falling sm stack b = length (foldl (fall sm) [b] stack) - 1

solve2 :: Input -> Int
solve2 bs = sum [num_falling sm stack b | b <- stack]
  where
    stack = sortOn bottom (drop_all bs)
    sm = support_map stack

tests2 :: [(String, Int)]
tests2 = [(testInput, 7)]

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
