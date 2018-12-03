-- Langton's Ant
module Main where

import Utilities
import Prelude hiding (Either(Left, Right))
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)

data NodeState = Clean | Weakened | Infected | Flagged
    deriving (Eq, Show)

-- Grid of node states: clean entries are omitted
type Grid = Map Position NodeState

get :: Position -> Grid -> NodeState
get pos g = Map.findWithDefault Clean pos g

set :: Position -> NodeState -> Grid -> Grid
set pos Clean g = Map.delete pos g
set pos state g = Map.insert pos state g

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

move :: Direction -> Position -> Position
move Up (x, y) = (x, y-1)
move Down (x, y) = (x, y+1)
move Left (x, y) = (x-1, y)
move Right (x, y) = (x+1, y)

data State = State {
    position :: !Position,
    direction :: !Direction,
    states :: !Grid,
    infections :: !Int }
    deriving Show

type Input = State

parse :: String -> Input
parse s = State {
    position = centre,
    direction = Up,
    states = grid,
    infections = 0 }
  where
    ls = lines s
    grid = Map.fromList [((x, y), Infected) |
        (y, line) <- zip [0..] ls,
        (x, c) <- zip [0..] line, c == '#']
    centre = (middle (length (head ls)), middle (length ls))
    middle n = (n-1) `div` 2

burst1 :: State -> State
burst1 (State pos dir g n) = State pos' dir' g' n'
  where
    infected = Map.member pos g
    dir' = (if infected then turnRight else turnLeft) dir
    g' = set pos (if infected then Clean else Infected) g
    pos' = move dir' pos
    n' = if infected then n else n+1

solve1 :: Input -> Int
solve1 = infections . times 10000 burst1

testInput :: String
testInput = "\
    \..#\n\
    \#..\n\
    \...\n"

tests1 :: [((Int, String), Int)]
tests1 = [((10000, testInput), 5587)]

-- Part Two

burst2 :: State -> State
burst2 (State pos dir g n) = State pos' dir' g' n'
  where
    state = get pos g
    dir' = turn state dir
    state' = newState state
    g' = set pos state' g
    pos' = move dir' pos
    n' = if state' == Infected then n+1 else n

turn :: NodeState -> Direction -> Direction
turn Clean = turnLeft
turn Weakened = id
turn Infected = turnRight
turn Flagged = turnLeft . turnLeft

newState :: NodeState -> NodeState
newState Clean = Weakened
newState Weakened = Infected
newState Infected = Flagged
newState Flagged = Clean

solve2 :: Input -> Int
solve2 = infections . times 10000000 burst2

tests2 :: [((Int, String), Int)]
tests2 = [((100, testInput), 26), ((10000000, testInput), 2511944)]

main :: IO ()
main = do
    s <- readFile "input22.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (runParsed burst1) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (runParsed burst2) tests2))
    print (solve2 input)

runParsed :: (State -> State) -> (Int, String) -> Int
runParsed f (n, s) = infections (times n f (parse s))
