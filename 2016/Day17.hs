module Main where

import Utilities
import Geometry
import Control.Monad
import Data.Functor
import Data.Maybe
import MD5

data Direction = U | D | L | R
  deriving (Show, Bounded, Enum)
type Path = [Direction]

showPath :: Path -> String
showPath = concatMap show

open :: String -> Path -> [Direction]
open passcode path = [d | (d, c) <- zip allValues hash, c >= 'b']
  where
    hash = md5s (passcode ++ showPath path)

x_max, y_max :: Int
x_max = 3
y_max = 3

move :: Direction -> Position -> Maybe Position
move U (Position x y) = guard (y > 0) $> Position x (y-1)
move D (Position x y) = guard (y < y_max) $> Position x (y+1)
move L (Position x y) = guard (x > 0) $> Position (x-1) y
move R (Position x y) = guard (x < x_max) $> Position (x+1) y

data State = State { position :: Position, history :: Path }
  deriving (Show)

start :: State
start = State zero []

finished :: State -> Bool
finished (State (Position x y) _) = x == x_max && y == y_max

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
