module Main where

import Utilities
import Geometry
import qualified RationalList as RL
import Data.Bits
import qualified Data.Map as Map

-- Input processing

type Input = Grid

-- 5x5 grids

-- bit-wise representation of a 5x5 grid
newtype Grid = Grid Int
    deriving (Eq, Ord, Show)

pointIndex :: Position -> Int
pointIndex (Position x y) = x + 5*y

member :: Grid -> Position -> Bool
member (Grid g) p = testBit g (pointIndex p)

grid :: [Position] -> Grid
grid ps = Grid (foldr (.|.) zeroBits [bit (pointIndex p) | p <- ps])

parse :: String -> Input
parse s = grid [p | (p, c) <- readGrid s, c == '#']

display :: Grid -> String
display g = showGrid '.' $
    Map.fromList [(p, if member g p then '#' else '.') | p <- allPoints]

rating :: Grid -> Int
rating (Grid g) = g

neighbours :: Position -> [Position]
neighbours pos = map (pos .+.) unitVectors

-- neighbours within the grid
localNeighbours :: Position -> [Position]
localNeighbours p =
    [Position x y | Position x y <- neighbours p,
        0 <= x && x < 5 && 0 <= y && y < 5]

allPoints :: [Position]
allPoints = [Position x y | y <- [0..4], x <- [0..4]]

-- Part One

basicRule :: Bool -> Int -> Bool
basicRule True count = count == 1
basicRule False count = count == 1 || count == 2

rule :: Grid -> Position -> Bool
rule g p = basicRule (member g p) count
  where
    count = length $ filter (member g) $ localNeighbours p

step :: Grid -> Grid
step g = grid $ filter (rule g) $ allPoints

solve1 :: Input -> Int
solve1 = rating . head . RL.repetend . RL.iterate step

testInput :: String
testInput =
    "....#\n\
    \#..#.\n\
    \#..##\n\
    \..#..\n\
    \#....\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 2129920)]

-- Part Two

-- next value for this grid, using those above and below it
localStep2 :: Grid -> Grid -> Grid -> Grid
localStep2 above this below =
    grid [p | p <- allPoints, p /= centre,
        basicRule (member this p) (ncount p)]
  where
    centre = Position 2 2
    ncount p = length $
        filter (member above) (outerNeighbours p) ++
        filter (member this) (filter (/= centre) $ localNeighbours p) ++
        filter (member below) (innerNeighbours p)

-- neighbours in the outer grid
outerNeighbours :: Position -> [Position]
outerNeighbours (Position x y) = sides ++ topBottom
  where
    sides = [Position x' 2 | x' <- outside x]
    topBottom = [Position 2 y' | y' <- outside y]

outside :: Int -> [Int]
outside n
  | n == 0 = [1]
  | n == 4 = [3]
  | otherwise = []

-- neighbours in the inner grid
innerNeighbours :: Position -> [Position]
innerNeighbours (Position x y)
  | y == 2 = [Position x' y' | x' <- inside x, y' <- [0..4]]
  | x == 2 = [Position x' y' | y' <- inside y, x' <- [0..4]]
  | otherwise = []

inside :: Int -> [Int]
inside n
  | n == 1 = [0]
  | n == 3 = [4]
  | otherwise = []

step2 :: [Grid] -> [Grid]
step2 gs =
    zipWith3 localStep2
        ([emptyGrid, emptyGrid] ++ gs)
        ([emptyGrid] ++ gs ++ [emptyGrid])
        (gs ++ [emptyGrid, emptyGrid])

emptyGrid :: Grid
emptyGrid = grid []

population :: [Grid] -> Int
population = sum . map (popCount . rating)

solve2 :: Input -> Int
solve2 g = population (times 200 step2 [g])

tests2 :: [(String, Int)]
tests2 = [(testInput, 99)]

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (population . times 10 step2 . (:[]) . parse) tests2))
    print (solve2 input)
