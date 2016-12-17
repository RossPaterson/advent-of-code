module Day17 where

import Utilities
import Data.Maybe
import Data.Hash.MD5(md5s, Str(..)) -- from MissingH package

data Direction = U | D | L | R
  deriving (Show, Bounded, Enum, Eq, Ord)
type Path = [Direction]

showPath :: Path -> String
showPath = concatMap show

open :: String -> Path -> [Direction]
open passcode path = [d | (d, c) <- zip allValues hash, c >= 'b']
  where
    hash = md5s (Str (passcode ++ showPath path))

data Position = Pos Int Int
  deriving (Show, Eq, Ord)

when :: Bool -> a -> Maybe a
when b x = if b then Just x else Nothing

move :: Direction -> Position -> Maybe Position
move U (Pos x y) = when (y > 0) (Pos x (y-1))
move D (Pos x y) = when (y < 3) (Pos x (y+1))
move L (Pos x y) = when (x > 0) (Pos (x-1) y)
move R (Pos x y) = when (x < 3) (Pos (x+1) y)

data State = State { position :: Position, history :: Path }
  deriving (Show, Eq, Ord)

start :: State
start = State (Pos 0 0) []

finished :: State -> Bool
finished (State (Pos x y) _) = x == 3 && y == 3

moves :: String -> State -> [State]
moves passcode (State pos path) =
    [State pos' (path ++ [d]) |
        d <- open passcode path, pos' <- maybeToList (move d pos)]

moves2 :: String -> State -> [State]
moves2 passcode state
  | finished state = []
  | otherwise = moves passcode state

solve :: String -> String
solve passcode =
    showPath $ history $ head $ dropWhile (not . finished) $
        concat $ bfs (moves passcode) [start]

test1 = solve "ihgpwlah"
test2 = solve "kglvqrro"
test3 = solve "ulqzkmiv"

input = "qljzarfv"

puzzle1 = putStrLn (solve input)

test4 = solve2 "ihgpwlah"
test5 = solve2 "kglvqrro"
test6 = solve2 "ulqzkmiv"

solve2 :: String -> Int
solve2 passcode =
    length $ history $ last $ filter finished $
        concat $ bfs (moves2 passcode) [start]

puzzle2 = print (solve2 input)
