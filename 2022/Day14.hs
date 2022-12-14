module Main where

import Utilities
import Geometry
import Parser
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Path]
type Path = [Position]

parse :: String -> Input
parse = map (runParser points) . lines
  where
    points = sepBy1 point (string " -> ")
    point = Position <$> nat <* char ',' <*> nat

-- Part One

source :: Position
source = Position 500 0

-- Properties of the empty cave

data Cave = Cave {
    rock :: Set Position,
    all_xs :: Set Int,
    max_y :: Int }
    deriving (Show)

mkCave :: [Path] -> Cave
mkCave ps = Cave {
    rock = rock_ps,
    all_xs = Set.fromList [x | Position x _ <- Set.elems rock_ps],
    max_y = maximum [y | path <- ps, Position _ y <- path]
    }
  where
    rock_ps = Set.unions (map pathPoints ps)

pathPoints :: Path -> Set Position
pathPoints ps = Set.unions (zipWith segment ps (tail ps))

segment :: Position -> Position -> Set Position
segment (Position x1 y1) (Position x2 y2)
  | x1 == x2 = Set.fromList [Position x1 y | y <- range y1 y2]
  | y1 == y2 = Set.fromList [Position x y1 | x <- range x1 x2]
  | otherwise = error "diagonal segment"

range :: Int -> Int -> [Int]
range a b
  | a <= b = [a..b]
  | otherwise = [b..a]

-- Where does a new grain of sand come to rest?
restPosition :: Cave -> Set Position -> Maybe Position
restPosition cave sand
  | null unsupported = Just (last path)
  | otherwise = Nothing
  where
    (path, unsupported) =
        span (supported cave) (iterateWhileJust (nextPosition blocked) source)
    material = Set.union (rock cave) sand
    blocked p = Set.member p material

-- Is there any rock directly below this position?
-- (If not, the grain will disappear into the abyss.)
supported :: Cave -> Position -> Bool
supported cave (Position x y) = Set.member x (all_xs cave) && y < max_y cave

-- First position below p that is not blocked, if any
nextPosition :: (Position -> Bool) -> Position -> Maybe Position
nextPosition blocked p =
    listToMaybe (dropWhile blocked [p .+. Position x 1 | x <- [0, -1, 1]])

-- Keep adding elements to a set until there is nothing to add
accumulate :: (Ord a) => (Set a -> Maybe a) -> Set a
accumulate mb_next = acc Set.empty
  where
    acc s = case mb_next s of
        Just p -> acc (Set.insert p s)
        Nothing -> s

solve1 :: Input -> Int
solve1 ps = Set.size (accumulate (restPosition (mkCave ps)))

testInput :: String
testInput = "\
    \498,4 -> 498,6 -> 496,6\n\
    \503,4 -> 502,4 -> 502,9 -> 494,9\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 24)]

-- Part Two

restPosition2 :: Cave -> Set Position -> Maybe Position
restPosition2 cave sand
  | Set.member source sand = Nothing
  | otherwise = Just (whileJust (nextPosition blocked) source)
  where
    material = Set.union (rock cave) sand
    floor_y = max_y cave + 2
    blocked p@(Position _ y) = Set.member p material || y == floor_y

solve2 :: Input -> Int
solve2 ps = Set.size (accumulate (restPosition2 (mkCave ps)))

tests2 :: [(String, Int)]
tests2 = [(testInput, 93)]

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
