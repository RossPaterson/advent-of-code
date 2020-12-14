-- Langton's Ant
module Main where

import Utilities
import Geometry
import Prelude hiding (Either(Left, Right))
import Data.Map (Map)
import qualified Data.Map as Map

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

move :: Direction -> Position
move Up = Position 0 (-1)
move Down = Position 0 1
move Left = Position (-1) 0
move Right = Position 1 0

data State = State {
    position :: !Position,
    direction :: !Direction,
    states :: !Grid }
    deriving Show

type Input = State

parse :: String -> Input
parse s = State {
    position = centre,
    direction = Up,
    states = grid }
  where
    ls = lines s
    grid = Map.fromList [(p, Infected) | (p, c) <- readGrid s, c == '#']
    centre = Position (middle (length (head ls))) (middle (length ls))
    middle n = (n-1) `div` 2

-- one step of the machine
burst :: (NodeState -> NodeState) ->
    (NodeState -> Direction -> Direction) -> State -> State
burst newState turn (State pos dir g) = State pos' dir' g'
  where
    node = get pos g
    node' = newState node
    dir' = turn node dir
    g' = set pos node' g
    pos' = pos .+. move dir'

newState1 :: NodeState -> NodeState
newState1 Infected = Clean
newState1 Clean = Infected
newState1 _ = error "unexpected node state"

turn1 :: NodeState -> Direction -> Direction
turn1 Infected = turnRight
turn1 Clean = turnLeft
turn1 _ = error "unexpected node state"

burst1 :: State -> State
burst1 = burst newState1 turn1

-- number of new infections in a sequence of states
infections :: [State] -> Int
infections ss =
    length [s' | (s, s') <- zip ss (tail ss),
        let pos = position s,
        get pos (states s) /= Infected,
        get pos (states s') == Infected]

solve1 :: Input -> Int
solve1 = infections . take 10001 . iterate burst1

testInput :: String
testInput =
    "..#\n\
    \#..\n\
    \...\n"

tests1 :: [((Int, String), Int)]
tests1 = [((10000, testInput), 5587)]

-- Part Two

burst2 :: State -> State
burst2 = burst newState2 turn2

newState2 :: NodeState -> NodeState
newState2 Clean = Weakened
newState2 Weakened = Infected
newState2 Infected = Flagged
newState2 Flagged = Clean

turn2 :: NodeState -> Direction -> Direction
turn2 Clean = turnLeft
turn2 Weakened = id
turn2 Infected = turnRight
turn2 Flagged = turnLeft . turnLeft

solve2 :: Input -> Int
solve2 = infections . take 10000001 . iterate burst2

tests2 :: [((Int, String), Int)]
tests2 = [((100, testInput), 26), ((10000000, testInput), 2511944)]

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (runParsed burst1) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (runParsed burst2) tests2))
    print (solve2 input)

runParsed :: (State -> State) -> (Int, String) -> Int
runParsed f (n, s) = infections (take (n+1) (iterate f (parse s)))
