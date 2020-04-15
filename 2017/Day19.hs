module Main where

import Prelude hiding (Either(Left, Right))
import Utilities
import Cartesian
import Control.Monad
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

type Diagram = Map Position Char
type Input = Diagram

parse :: String -> Input
parse s = Map.fromList [(p, c) | (p, c) <- readGrid s, c /= ' ']

data Direction = Up | Down | Left | Right
    deriving Show

turnLeft :: Direction -> Direction
turnLeft Up = Left
turnLeft Left = Down
turnLeft Down = Right
turnLeft Right = Up

turnRight :: Direction -> Direction
turnRight Up = Right
turnRight Right = Down
turnRight Down = Left
turnRight Left = Up

direction :: Direction -> Position
direction Up = Position 0 (-1)
direction Down = Position 0 1
direction Left = Position (-1) 0
direction Right = Position 1 0

type State = (Position, Direction)

-- start above the uppermost wire, so it is included in the path
start :: Map Position a -> State
start diagram = (minimumBy cmpRow (Map.keys diagram) .+. direction Up, Down)
  where
    cmpRow (Position _ y1) (Position _ y2) = compare y1 y2

step :: Map Position a -> State -> Maybe (a, State)
step diagram (pos, dir) =
    -- move in the same direction if we can, otherwise try turning
    msum [moveto (pos .+. direction d) d |
        d <- [dir, turnLeft dir, turnRight dir]]
  where
    moveto pos' dir' = do
        cell <- Map.lookup pos' diagram
        return (cell, (pos', dir'))

path :: Map Position a -> [a]
path diagram = unfoldr (step diagram) (start diagram)

solve1 :: Input -> String
solve1 = filter isAlpha . path

testInput :: String
testInput =
    "     |          \n\
    \     |  +--+    \n\
    \     A  |  C    \n\
    \ F---|----E|--+ \n\
    \     |  |  |  D \n\
    \     +B-+  +--+ \n\
    \\n"


tests1 :: [(String, String)]
tests1 = [(testInput, "ABCDEF")]

-- Part Two

solve2 :: Input -> Int
solve2 = length . path

tests2 :: [(String, Int)]
tests2 = [(testInput, 38)]

main :: IO ()
main = do
    s <- readFile "input/19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
