-- alternative solution using depth first search with cloned droids
module Main where

import Utilities
import Graph
import Geometry
import Intcode
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

data Move = N | S | W | E
    deriving (Bounded, Enum, Eq, Ord, Show)

fromMove :: Move -> Value
fromMove d = toValue d + 1

startPoint :: Position
startPoint = zero

move :: Move -> Position
move N = Position 0 (-1)
move S = Position 0 1
move W = Position (-1) 0
move E = Position 1 0

data Response = Blocked | Moved | Found
    deriving (Enum, Show)

toResponse :: Value -> Response
toResponse = fromValue

data Cell = Wall | Space
    deriving (Eq, Show)

showCell :: Cell -> Char
showCell Wall = '#'
showCell Space = '.'

data Maze = Maze {
    target :: Maybe Position,
    maze_map :: Map Position Cell
    }

initMaze :: Maze
initMaze = Maze {
    target = Nothing,
    maze_map = Map.singleton startPoint Space
    }

showMaze :: Maze -> String
showMaze maze = showGrid ' ' $
    target_map `Map.union`
    Map.singleton startPoint '0' `Map.union`
    fmap showCell (maze_map maze)
  where
    target_map = case target maze of
        Just p -> Map.singleton p '*'
        Nothing -> Map.empty

-- map the maze using the droid program
mapMaze :: Memory -> Maze
mapMaze mem = searchMaze startPoint (automaton mem) initMaze

-- search from p with a droid positioned at p
searchMaze :: Position -> Automaton -> Maze -> Maze
searchMaze p droid m =
    foldl (moveTo droid) m [(d, p .+. move d) | d <- allValues]

-- consider droid moves to adjacent points
moveTo :: Automaton -> Maze -> (Move, Position) -> Maze
moveTo droid maze (d, p)
  | Map.member p m = maze
  | otherwise = case r of
        Blocked ->
            maze { maze_map = Map.insert p Wall m }
        Moved -> searchMaze p droid' $
            maze { maze_map = Map.insert p Space m }
        Found -> searchMaze p droid' $
            maze { maze_map = Map.insert p Space m, target = Just p }
  where
    (r, droid') = moveDroid droid d
    m = maze_map maze

-- interact with the droid, attempting to make the move
moveDroid :: Automaton -> Move -> (Response, Automaton)
moveDroid (ReadValue k) d = case k (fromMove d) of
    WriteValue v droid -> (toResponse v, droid)
    _ -> error "Droid failed to respond"
moveDroid _ _ = error "Droid not accepting input"

-- destinations reachable in one step from p
neighbours :: Map Position Cell -> Position -> [Position]
neighbours m p =
    [p' | d <- allValues, let p' = p .+. move d, Map.lookup p' m == Just Space]

solve1 :: Maze -> Int
solve1 maze = case target maze of
    Nothing -> error "Target not found"
    Just t ->
        length $ takeWhile (t `notElem`) $
            bfs (neighbours (maze_map maze)) [startPoint]

-- Part Two

solve2 :: Maze -> Int
solve2 maze = case target maze of
    Nothing -> error "Target not found"
    Just t -> length (bfs (neighbours (maze_map maze)) [t]) - 1

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let maze = mapMaze (parse s)
    -- putStr (showMaze maze)
    print (solve1 maze)
    print (solve2 maze)
