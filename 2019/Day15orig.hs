module Main where

import Utilities
import Geometry
import Intcode
import Data.Maybe
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

move :: Position -> Move -> Position
move (Position x y) N = Position x (y-1)
move (Position x y) S = Position x (y+1)
move (Position x y) W = Position (x-1) y
move (Position x y) E = Position (x+1) y

data Response = Blocked | Moved | Found
    deriving (Enum, Show)

toResponse :: Value -> Response
toResponse = fromValue

data Cell = Wall | Space
    deriving (Eq, Show)

showCell :: Cell -> Char
showCell Wall = '#'
showCell Space = '.'

data Droid = Droid {
    position :: Position,
    target :: Maybe Position,
    maze_map :: Map Position Cell
    }

initDroid :: Droid
initDroid = Droid {
    position = zero,
    target = Nothing,
    maze_map = Map.singleton startPoint Space
    }

showDroid :: Droid -> String
showDroid d = showGrid ' ' $
    target_map `Map.union`
    Map.singleton (position d) 'D' `Map.union`
    Map.singleton startPoint '0' `Map.union`
    fmap showCell (maze_map d)
  where
    target_map = case target d of
        Just p -> Map.singleton p '*'
        Nothing -> Map.empty

-- Minimal paths from a given start node.
-- Each node reachable from start appears in exactly one list, paired with
-- a minimal path to that node.  All paths in the kth list have length k.
minimalPaths :: Ord n => (n -> [(e, n)]) -> n -> [[(n, [e])]]
minimalPaths g start = allPaths Map.empty (Map.singleton start [])
  where
    allPaths done fringe
      | Map.null fringe = []
      | otherwise = Map.assocs (fmap reverse fringe) : allPaths done' fringe'
      where
        done' = Map.union done fringe
        fringe' =
            Map.fromList [(n', e:path) |
                (n, path) <- Map.assocs fringe, (e, n') <- g n]
            `Map.difference` done'

-- minimal path between to points in the maze
pathBetween :: Map Position Cell  -> Position -> Position -> [Move]
pathBetween m src dest =
    head [path | (p, path) <- concat (minimalPaths (steps m) src), p == dest]

-- moves and destinations the droid can reach in one step from p
steps :: Map Position Cell -> Position -> [(Move, Position)]
steps m p =
    [(d, p') | d <- allValues, let p' = move p d, Map.lookup p' m /= Just Wall]

-- path from droid's location to the closest unknown cell (if any)
unknown :: Droid -> Maybe [Move]
unknown d = listToMaybe [path |
    (p, path) <- concat (minimalPaths (steps maze) (position d)),
    Map.lookup p maze == Nothing]
  where
    maze = maze_map d

-- interact with the program to create a complete map of the maze
exploreMaze :: Memory -> Droid
exploreMaze mem = lastLP history
  where
    responses = map toResponse $ streamFunction mem $ map fromMove moves
    droids = scanl addResponse initDroid (zip moves responses)
    moves = concat (initLP history)
    history = drive droids

-- update the droid and maze from a move and its response
addResponse :: Droid -> (Move, Response) -> Droid
addResponse d (dir, r) = case r of
    Blocked -> d { maze_map = Map.insert p Wall maze }
    Moved -> d { maze_map = Map.insert p Space maze, position = p }
    Found ->
        d { maze_map = Map.insert p Space maze, position = p, target = Just p }
  where
    p = move (position d) dir
    maze = maze_map d

-- Produce a sequence of paths, each moving the droid to an unknown cell,
-- and return the final state when no more unknown cells are reachable.
drive :: [Droid] -> ListPlus [Move] Droid
drive (d:ds) = case unknown d of
    Nothing -> End d
    Just [] -> error "Empty path"
    Just ms ->
        let n = length ms - 1 in
        -- ignore states until but the last move is executed
        Cons ms (drive (drop n ds))
drive [] = error "No more droids"

-- minimal path from the origin to the target
minimal :: Droid -> [Move]
minimal d = case target d of
    Nothing -> error "No target"
    Just t -> pathBetween maze zero t
  where
    maze = maze_map d

solve1 :: Input -> Int
solve1 = length . minimal . exploreMaze

-- Part Two

solve2 :: Input -> Int
solve2 mem = length (minimalPaths (steps (maze_map d)) t) - 1
  where
    d = exploreMaze mem
    Just t = target d

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
