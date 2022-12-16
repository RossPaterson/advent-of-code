module Main where

import Utilities
import Geometry
import Parser

-- Input processing

type Input = [Sensor]
data Sensor = Sensor Position Position
    deriving (Show)

parse :: String -> Input
parse = map (runParser sensor) . lines
  where
    sensor = Sensor <$ string "Sensor at " <*> point <*
        string ": closest beacon is at " <*> point
    point = Position <$ string "x=" <*> int <* string ", y=" <*> int

-- Part One

data Interval = Interval Int Int
    deriving (Show)

-- disjoint intervals in ascending order
type IntervalList = [Interval]

sizeInterval :: Interval -> Int
sizeInterval (Interval b e) = e-b+1

sizeIntervals :: IntervalList -> Int
sizeIntervals = sum . map sizeInterval

memberInterval :: Int -> IntervalList -> Bool
memberInterval x is = or [b <= x && x <= e | Interval b e <- is]

addInterval :: Interval -> IntervalList -> IntervalList
addInterval i [] = [i]
addInterval i1@(Interval b1 e1) (i2@(Interval b2 e2):is)
  | e1 < b2-1 = i1:i2:is
  | e2 < b1-1 = i2:addInterval i1 is
  | otherwise = addInterval (Interval (min b1 b2) (max e1 e2)) is

sensorInterval :: Int -> Sensor -> Maybe Interval
sensorInterval y (Sensor s@(Position sx sy) b)
  | delta >= 0 = Just (Interval (sx-delta) (sx+delta))
  | otherwise = Nothing
  where
    delta = distance s b - abs (sy-y)

sensorsInterval :: Int -> [Sensor] -> IntervalList
sensorsInterval y = foldr add []
  where
    add sensor = maybe id addInterval (sensorInterval y sensor)

beacons :: [Sensor] -> [Position]
beacons sensors = fastNub [b | Sensor _ b <- sensors]

solve1 :: Int -> Input -> Int
solve1 y sensors =
    sizeIntervals (sensorsInterval y sensors) -
        length [bx | Position bx by <- beacons sensors, by == y]

testInput :: String
testInput = "\
    \Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
    \Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
    \Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
    \Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
    \Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
    \Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
    \Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
    \Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
    \Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
    \Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
    \Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
    \Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
    \Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
    \Sensor at x=20, y=1: closest beacon is at x=15, y=3\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 26)]

-- Part Two

covered :: [Sensor] -> Position -> Bool
covered sensors p = or [distance s p <= distance s b | Sensor s b <- sensors]

-- rhombus delimited by top and bottom corners,
-- closed at the top and open at the bottom
data Region = Region Diagonal Diagonal
    deriving (Show)

-- region including the two points
region :: Position -> Position -> Region
region top bottom =
    Region (positionToDiagonal top)
        (positionToDiagonal (bottom .+. Position 0 1))

-- Is the region empty?
nullRegion :: Region -> Bool
nullRegion (Region (Diagonal ta tb) (Diagonal ba bb)) = ta >= ba || tb >= bb

-- All the positions inside a region
contents :: Region -> [Position]
contents (Region (Diagonal ta tb) (Diagonal ba bb)) =
    [p | a <- [ta..ba-1], b <- [tb..bb-1],
        At p <- [diagonalPosition (Diagonal a b)]]

-- The region centred on a sensor and extending out to its closest beacon
sensorRegion :: Sensor -> Region
sensorRegion (Sensor s b) = region (s .-. offset) (s .+. offset)
  where
    offset = Position 0 (distance s b)

-- Subtracting one region from another produces up to four new regions:
--
--       /\
--      /  \
--     /\   \
--    /  \   \
--   /   /\   \
--  /\  /XX\   \
-- /  \/XXX/\  /
-- \   \XX/  \/
--  \   \/   /
--   \   \  /
--    \   \/
--     \  /
--      \/
--
differenceRegion :: Region -> Region -> [Region]
differenceRegion (Region (Diagonal t1r t1l) (Diagonal b1r b1l))
        (Region (Diagonal t2r t2l) (Diagonal b2r b2l)) =
    filter (not . nullRegion) [
        Region (Diagonal t1r t1l) (Diagonal b1r (min t2l b1l)),
        Region (Diagonal t1r (max b2l t1l)) (Diagonal b1r b1l),
        Region (Diagonal t1r tl) (Diagonal (min t2r b1r) bl),
        Region (Diagonal (max t1r b2r) tl) (Diagonal b1r bl)]
  where
    tl = max t1l t2l
    bl = min b1l b2l

-- is any of the region in the search area [0..n]x[0..n] ?
candidate :: Int -> Region -> Bool
candidate n (Region (Diagonal ta tb) (Diagonal ba bb)) =
    0 <= max_x && min_x <= n && 0 <= max_y && min_y <= n
  where
    min_x = (ta - bb) `div` 2     -- Diagonal ta bb
    max_x = (ba - tb + 1) `div` 2 -- Diagonal ba tb
    min_y = (ta + tb) `div` 2     -- Diagonal ta tb
    max_y = (ba + bb + 1) `div` 2 -- Diagonal ba bb

-- region containing the search area
boundingRegion :: Int -> Region
boundingRegion n = region (Position x (-x)) (Position x (n+x))
  where
    x = n `div` 2

search :: Int -> Region -> [Region] -> [Region]
search n sr rs =
    [piece | r <- rs, piece <- differenceRegion r sr, candidate n piece]

solutions :: Int -> Input -> [Position]
solutions n sensors =
    [p | r <- foldr (search n) [boundingRegion n] (map sensorRegion sensors),
        p@(Position x y) <- contents r,
        0 <= x && x <= n && 0 <= y && y <= n]

tuningFrequency :: Position -> Int
tuningFrequency (Position x y) = 4000000*x + y

solve2 :: Int -> Input -> Int
solve2 n sensors = tuningFrequency $ head $ solutions n sensors

tests2 :: [(String, Int)]
tests2 = [(testInput, 56000011)]

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 10 . parse) tests1))
    print (solve1 2000000 input)
    putStr (unlines (failures "solve2" (solve2 20 . parse) tests2))
    print (solve2 4000000 input)
