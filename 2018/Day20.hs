module Main where

import Utilities
import Graph
import Parser
import Control.Applicative
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Maze

-- steps to be taken in sequence
type Expr = [Step]
data Step = Simple Direction | Branch [Expr]
  deriving Show

data Direction = N | E | S | W
  deriving (Show, Bounded, Enum)

parse :: String -> Input
parse = makeMaze . runParser top . filter (/= '\n')
  where
    top = char '^' *> expr <* char '$'
    expr = many step
    step =
        Simple <$> enumValue <|>
        Branch <$ char '(' <*> sepBy1 expr (char '|') <* char ')'

type Room = (Int, Int)
-- door on the east or south side of a room
data Door = East Room | South Room
  deriving (Show, Eq, Ord)

type Maze = Set Door

-- string representation of a maze, as in the puzzle statement
showMaze :: Maze -> String
showMaze m = unlines $
    replicate (2*(xmax - xmin) + 3) '#' :
        concat [[ewRow y, nsRow y] | y <- [ymin..ymax]]
  where
    ps = concatMap rooms (Set.toList m)
    xmin = minimum (map fst ps)
    xmax = maximum (map fst ps)
    ymin = minimum (map snd ps)
    ymax = maximum (map snd ps)
    ewRow y = concat ['#':concat [[showRoom p, eastDoor p] | p <- row y]]
    nsRow y = concat ['#':concat [[southDoor p, '#'] | p <- row y]]
    row y = [(x, y) | x <- [xmin..xmax]]
    showRoom p
      | p == (0, 0) = 'X'
      | otherwise = '.'
    eastDoor p
      | Set.member (East p) m = '|'
      | otherwise = '#'
    southDoor p
      | Set.member (South p) m = '-'
      | otherwise = '#'

-- rooms adjacent to a door
rooms :: Door -> [Room]
rooms (East (x, y)) = [(x, y), (x+1, y)]
rooms (South (x, y)) = [(x, y), (x, y+1)]

-- make a maze from all the paths in the expression
makeMaze :: Expr -> Maze
makeMaze = snd . walkExpr (Set.singleton (0, 0))

-- follow directions from a set of rooms in parallel
-- (Exploring in parallel will be more efficient if different choices
-- in a branch end at the same position.)
walkExpr :: Set Room -> Expr -> (Set Room, Maze)
walkExpr ps ss = (ps', Set.unions ms)
  where
    (ps', ms) = mapAccumL walkStep ps ss

-- take one step from a set of rooms in parallel
walkStep :: Set Room -> Step -> (Set Room, Maze)
walkStep ps (Simple dir) = (Set.fromList ps', Set.fromList doors)
  where
    (doors, ps') = unzip [move dir p | p <- Set.toList ps]
walkStep ps (Branch es) = (Set.unions pss, Set.unions ms)
  where
    (pss, ms) = unzip (map (walkExpr ps) es)

-- move from a room, returning the door gone through and the new room
move :: Direction -> Room -> (Door, Room)
move N (x, y) = (South (x, y-1), (x, y-1))
move E (x, y) = (East (x, y), (x+1, y))
move S (x, y) = (South (x, y), (x, y+1))
move W (x, y) = (East (x-1, y), (x-1, y))

-- Part One

-- number of steps from the origin to the furthest room
solve1 :: Input -> Int
solve1 m = length (reachable m) - 1

-- lists of rooms reachable from the origin in each number of steps
reachable :: Maze -> [[Room]]
reachable m = bfs (neighbours m) [(0, 0)]

-- adjacent rooms reachable through a door
neighbours :: Maze -> Room -> [Room]
neighbours m p =
    [p' | dir <- allValues, let (door, p') = move dir p, Set.member door m]

tests1 :: [(String, Int)]
tests1 = [
    ("^WNE$", 3),
    ("^ENWWW(NEEE|SSE(EE|N))$", 10),
    ("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$", 18),
    ("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", 23),
    ("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", 31)]

-- Part Two

-- number of rooms at least 1000 steps from the origin
solve2 :: Input -> Int
solve2 = sum . map length . drop 1000 . reachable

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
