module Main where

import Geometry
import Parser
import Utilities
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Set as Set

-- Input processing

type Input = [Position]

parse :: String -> Input
parse = map (runParser coord) . lines
  where
    coord = Position <$> nat <* string ", " <*> nat

-- Part One

solve1 :: Input -> Int
solve1 = maximum . map snd . areas

-- a rectangular area
data Rect = Rect { xmin :: Int, xmax :: Int, ymin :: Int, ymax :: Int }

-- smallest rectangle containing all the coordinates
boundingRect :: [Position] -> Rect
boundingRect ps = Rect {
    xmin = minimum [x | Position x _ <- ps],
    xmax = maximum [x | Position x _ <- ps],
    ymin = minimum [y | Position _ y <- ps],
    ymax = maximum [y | Position _ y <- ps]
    }

-- all the coordinates in a rectangle
allCoords :: Rect -> [Position]
allCoords r = [Position x y | x <- [xmin r..xmax r], y <- [ymin r..ymax r]]

-- coordinates around the outside of a rectangle
boundary :: Rect -> [Position]
boundary r =
    [Position (xmin r-1) y | y <- [ymin r-1..ymax r+1]] ++
    [Position (xmax r+1) y | y <- [ymin r-1..ymax r+1]] ++
    [Position x (ymin r-1) | x <- [xmin r..xmax r]] ++
    [Position x (ymax r+1) | x <- [xmin r..xmax r]]

-- coordinates with finite areas and their sizes
-- (relies on the fact that all finite areas are within the rectangle)
areas :: [Position] -> [(Position, Int)]
areas cs =
    frequency $ filter (not . flip Set.member unbounded) $ closestAll points cs
  where
    rect = boundingRect cs
    points = allCoords rect
    unbounded = Set.fromList $ closestAll (boundary rect) cs

-- unique closest coordinate to each coordinate in ps
closestAll :: [Position] -> [Position] -> [Position]
closestAll ps cs = [c | p <- ps, c <- maybeToList (closest p cs)]

-- the unique closest coordinate to p
closest :: Position -> [Position] -> Maybe Position
closest p cs =
    unique $ map fst $ head $ groupBy (same snd) $ sortBy (comparing snd) $
    [(c, distance p c) | c <- cs]
  where
    unique [x] = Just x
    unique _ = Nothing

testInput :: String
testInput =
    "1, 1\n\
    \1, 6\n\
    \8, 3\n\
    \3, 4\n\
    \5, 5\n\
    \8, 9\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 17)]

-- Part Two

solve2 :: Input -> Int
solve2 = numWithin 10000

-- number of coordinates within n of all of cs
numWithin :: Int -> [Position] -> Int
numWithin n cs = length $ filter (< n) $ map (totalDistance cs) points
  where
    rect = boundingRect cs
    big_rect = expand ((n + ncs - 1) `div` ncs) rect
    ncs = length cs
    points = allCoords big_rect

-- make a rectangle bigger by n in all directions
expand :: Int -> Rect -> Rect
expand n r = Rect {
    xmin = xmin r - n,
    xmax = xmax r + n,
    ymin = ymin r - n,
    ymax = ymax r + n
    }

-- total distance to all of cs
totalDistance :: [Position] -> Position -> Int
totalDistance cs p = sum [distance p c | c <- cs]

tests2 :: [((Int, String), Int)]
tests2 = [((32, testInput), 16)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (uncurry numWithin . fmap parse) tests2))
    print (solve2 input)
