module Main where

import Utilities
import Data.Ord
import Data.List

-- Input processing

type Input = Int

-- Part One

solve1 :: Input -> Position
solve1 = largest 3 300

-- nxn square with the largest sum
largest :: Int -> Int -> Int -> Position
largest n size serialno =
    fst $ maximumBy (comparing snd)
    [((x, y), power x y) | x <- [1.. size-n-1], y <- [1..size-n-1]]
  where
    power x y =
        sum [powerLevel serialno x' y' |
            x' <- [x..x+n-1], y' <- [y..y+n-1]]

type Position = (Int, Int)

-- hash function computing a power level for a square in the grid
powerLevel :: Int -> Int -> Int -> Int
powerLevel serialno x y =
    (rackID * y + serialno) * rackID `mod` 1000 `div` 100 - 5
  where
    rackID = x + 10

tests1 :: [(Int, Position)]
tests1 = [(18, (33,45)), (42, (21,61))]

-- Part Two

type SquareID = (Int, Int, Int)

solve2 :: Input -> SquareID
solve2 = largestAny 300

-- square of any size with the largest sum
largestAny :: Int -> Int -> SquareID
largestAny size serialno = fst $
    maximumBy (comparing snd) $
    concatMap squareSums $
    takeWhile (not . finished) $
    iterate (nextState g0) $
    initState g0
  where
    g0 = initGrid size serialno

-- rectangular grid of values
type Grid = [[Int]]

-- add two grids pointwise
plus :: Grid -> Grid -> Grid
plus = zipWith (zipWith (+))

-- cut edges off a grid

cutLeft :: Grid -> Grid
cutLeft = map tail

cutRight :: Grid -> Grid
cutRight = map init

cutTop :: Grid -> Grid
cutTop = tail

cutBottom :: Grid -> Grid
cutBottom = init

initGrid :: Int -> Int -> Grid
initGrid size serialno =
    [[powerLevel serialno x y | x <- [1..size]] | y <- [1..size]]

-- Dynamic programming approach to computing the total for each square:
--
-- For n > 1, the total of values of the square x,y,n is equal to the sum of
-- * the total of x+1,y+1,n-1
-- * the total of the subrow of length n starting at x,y
-- * the total of the subcolumn of length n-1 starting at x,y+1
--
-- The totals of the subrows and subcolumns can be built up similarly.

-- Grids of totals
data State = State {
    squareSize :: !Int,
    squareTotals :: Grid, -- sums of squares of size nxn starting at x,y
    subrowSums :: Grid, -- sums of subrows of length n starting at x,y
    subcolumnSums :: Grid -- sums of subcolumns of length n starting at x,y
    }
  deriving Show

initState :: Grid -> State
initState grid = State {
    squareSize = 1,
    squareTotals = grid,
    subrowSums = grid,
    subcolumnSums = grid
    }

finished :: State -> Bool
finished = null . squareTotals

-- Increase the squareSize by one and compute the totals for that size.
-- The resulting grids will be smaller by one in each direction.
nextState :: Grid -> State -> State
nextState g (State {
        squareSize = n, squareTotals = squares,
        subrowSums = subrows, subcolumnSums = subcols }) = State {
    squareSize = n + 1,
    squareTotals = squares',
    subrowSums = subrows',
    subcolumnSums = subcols'
    }
  where
    squares' =
        cutLeft (cutTop squares) `plus` subrows' `plus`
        cutTop (cutRight subcols)
    subrows' = g `plus` cutLeft (cutBottom subrows)
    subcols' = g `plus` cutTop (cutRight subcols)

-- square identifiers and their totals
squareSums :: State -> [(SquareID, Int)]
squareSums s =
    [((x, y, n), p) |
        (y, ps) <- zip [1..] (squareTotals s),
        (x, p) <- zip [1..] ps]
  where
    n = squareSize s

tests2 :: [(Int, SquareID)]
tests2 = [(18, (90,269,16)), (42, (232,251,12))]

main :: IO ()
main = do
    let input = 8772
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 input)
