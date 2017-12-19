module Main where

import Prelude hiding (Either(Left, Right))
import Utilities
import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

type Item = Char
data Cell = Wire | Item Item

data Position = Position Int Int -- row first, so start is first key
    deriving (Eq, Ord, Show)

type Diagram = Map Position Cell
type Input = Diagram

parse :: String -> Input
parse s = compose [Map.insert (Position row col) (cell c) |
    (row, line) <- zip [0..] (lines s),
    (col, c) <- zip [0..] line,
    c /= ' '] Map.empty
  where
    cell c | isAlpha c = Item c
    cell _ = Wire

data Direction = Up | Down | Left | Right
    deriving Show

move :: Direction -> Position -> Position
move Up (Position r c) = Position (r-1) c
move Down (Position r c) = Position (r+1) c
move Left (Position r c) = Position r (c-1)
move Right (Position r c) = Position r (c+1)

turns :: Direction -> [Direction]
turns Up = [Left, Right]
turns Down = [Left, Right]
turns Left = [Up, Down]
turns Right = [Up, Down]

-- (current position, current direction, items seen in reverse order)
type State = (Position, Direction, [Item])

start :: Diagram -> State
start diagram = (head (Map.keys diagram), Down, [])

record :: Cell -> [Item] -> [Item]
record (Item c) s = c:s
record _ s = s

step :: Diagram -> State -> Maybe State
step diagram (pos, dir, seen) =
    -- move in the same direction if we can, otherwise try turning
    msum [moveto (move d pos) d | d <- dir:turns dir]
  where
    moveto pos' dir' = do
        cell <- Map.lookup pos' diagram
        return (pos', dir', record cell seen)

solve1 :: Input -> [Item]
solve1 diagram = reverse seen
  where
    (pos, dir, seen) = whileJust (step diagram) (start diagram)

testInput :: String
testInput = "\
    \     |          \n\
    \     |  +--+    \n\
    \     A  |  C    \n\
    \ F---|----E|--+ \n\
    \     |  |  |  D \n\
    \     +B-+  +--+ \n\
    \\n"


tests1 :: [(String, [Item])]
tests1 = [(testInput, "ABCDEF")]

-- Part Two

solve2 :: Input -> Int
solve2 diagram = length (iterateWhileJust (step diagram) (start diagram))

tests2 :: [(String, Int)]
tests2 = [(testInput, 38)]

main :: IO ()
main = do
    s <- readFile "input19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
