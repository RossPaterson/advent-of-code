module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Set hiding (filter, foldl, map)

data Direction = Dir Int Int -- (North, East)
    deriving (Show, Eq, Ord)

-- Manhattan metric
measure :: Direction -> Int
measure (Dir n e) = abs n + abs e

scale :: Int -> Direction -> Direction
scale s (Dir n e) = Dir (s*n) (s*e)

add ::  Direction -> Direction -> Direction
add (Dir n1 e1) (Dir n2 e2) = Dir (n1+n2) (e1+e2)

north :: Direction
north = Dir 1 0

origin :: Direction
origin = Dir 0 0

data Turn = L | R
    deriving Show

turn :: Turn -> Direction -> Direction
turn L (Dir n e) = Dir e (-n)
turn R (Dir n e) = Dir (-e) n

data State = State { orientation :: Direction, position :: Direction }
data Move = Move Turn Int
type Input = [Move]

parse :: String -> Input
parse = runParser move_cmds
  where
    move_cmds = move_cmd `sepBy1` (char ',' <* many space) <* many space
    move_cmd = Move <$> (char 'L' *> pure L <|> char 'R' *> pure R) <*> nat

start :: State
start = State { orientation = north, position = origin }

move :: State -> Move -> State
move (State dir pos) (Move t s) = State dir' pos'
  where
    dir' = turn t dir
    pos' = add (scale s dir') pos

moves :: [Move] -> Direction
moves = position . foldl move start

solve1 :: Input -> Int
solve1 = measure . moves

tests1 :: [(String, Int)]
tests1 = [
    ("R2, L3", 5),
    ("R2, R2, R2", 2),
    ("R5, L5, R5, R3", 12)]


-- Part Two

type History = [Direction]

walk :: Int -> Direction -> Direction -> History
walk s dir pos = [add (scale i dir) pos | i <- [1..s]]

data State2 = State2 { orientation2 :: Direction, position2 :: Direction, history :: History }

start2 :: State2
start2 = State2 { orientation2 = north, position2 = origin, history = [origin] }

move2 :: State2 -> Move -> State2
move2 (State2 dir pos _) (Move t s) = State2 dir' pos' (walk s dir' pos)
  where
    dir' = turn t dir
    pos' = add (scale s dir') pos

visits :: [Move] -> History
visits = concat . map history . scanl move2 start2

initSets :: Ord a => [a] -> [Set a]
initSets = scanl (flip insert) Data.Set.empty

firstRepeated :: Ord a => [a] -> a
firstRepeated xs = head [x | (seen, x) <- zip (initSets xs) xs, member x seen]

solve2 :: Input -> Int
solve2 = measure . firstRepeated . visits

tests2 :: [(String, Int)]
tests2 = [("R8, R4, R4, R8", 4)]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
