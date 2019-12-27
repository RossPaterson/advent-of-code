module Main where

import Utilities
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Hash.MD5(md5s, Str(..)) -- from MissingH package

data Direction = U | D | L | R
  deriving (Show, Bounded, Enum)
type Path = [Direction]

showPath :: Path -> String
showPath = concatMap show

open :: String -> Path -> [Direction]
open passcode path = [d | (d, c) <- zip allValues hash, c >= 'b']
  where
    hash = md5s (Str (passcode ++ showPath path))

data Position = Pos Int Int
  deriving (Show)

x_max, y_max :: Int
x_max = 3
y_max = 3

move :: Direction -> Position -> Maybe Position
move U (Pos x y) = guard (y > 0) $> Pos x (y-1)
move D (Pos x y) = guard (y < y_max) $> Pos x (y+1)
move L (Pos x y) = guard (x > 0) $> Pos (x-1) y
move R (Pos x y) = guard (x < x_max) $> Pos (x+1) y

data State = State { position :: Position, history :: Path }
  deriving (Show)

start :: State
start = State (Pos 0 0) []

finished :: State -> Bool
finished (State (Pos x y) _) = x == x_max && y == y_max

moves :: String -> State -> [State]
moves passcode (State pos path) =
    [State pos' (path ++ [d]) |
        d <- open passcode path, pos' <- maybeToList (move d pos)]

solve1 :: String -> String
solve1 passcode =
    showPath $ history $ head $ filter finished $
        concat $ simple_bfs (moves passcode) [start]

-- a simpler version of breadth-first search suffices,
-- because duplicates cannot occur in this application
simple_bfs :: (a -> [a]) -> [a] -> [[a]]
simple_bfs f = takeWhile (not . null) . iterate (concatMap f)

tests1 :: [(String, String)]
tests1 = [
    ("ihgpwlah", "DDRRRD"),
    ("kglvqrro", "DDUDRLRRUDRD"),
    ("ulqzkmiv", "DRURDRUDDLLDLUURRDULRLDUUDDDRR")]

-- Part Two

moves2 :: String -> State -> [State]
moves2 passcode state
  | finished state = []
  | otherwise = moves passcode state

solve2 :: String -> Int
solve2 passcode =
    length $ history $ last $ filter finished $
        concat $ simple_bfs (moves2 passcode) [start]

tests2 :: [(String, Int)]
tests2 = [
    ("ihgpwlah", 370),
    ("kglvqrro", 492),
    ("ulqzkmiv", 830)]

input :: String
input = "qljzarfv"

main :: IO ()
main = do
    putStr (unlines (failures "solve1" solve1 tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 input)
