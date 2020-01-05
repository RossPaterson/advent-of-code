module Main where

import Cartesian
import Utilities
import Parser
import Control.Applicative
import Data.Set hiding (filter, foldl, map)

data Turn = L | R
    deriving (Bounded, Enum, Show)

turn :: Turn -> Point2 -> Point2
turn L (Point2 e n) = Point2 (-n) e
turn R (Point2 e n) = Point2 n (-e)

data State = State { orientation :: Point2, position :: Point2 }
data Move = Move Turn Int
type Input = [Move]

parse :: String -> Input
parse = runParser move_cmds
  where
    move_cmds = move_cmd `sepBy1` (char ',' <* many space) <* many space
    move_cmd = Move <$> enumValue <*> nat

north :: Point2
north = Point2 0 1

start :: State
start = State { orientation = north, position = zero }

move :: State -> Move -> State
move (State dir pos) (Move t s) = State dir' pos'
  where
    dir' = turn t dir
    pos' = pos .+. s *. dir'

moves :: [Move] -> Point2
moves = position . foldl move start

solve1 :: Input -> Int
solve1 = norm . moves

tests1 :: [(String, Int)]
tests1 = [
    ("R2, L3", 5),
    ("R2, R2, R2", 2),
    ("R5, L5, R5, R3", 12)]

-- Part Two

type History = [Point2]

walk :: Int -> Point2 -> Point2 -> History
walk s dir pos = [pos .+. i *. dir | i <- [1..s]]

data State2 = State2 { orientation2 :: Point2, position2 :: Point2, history :: History }

start2 :: State2
start2 = State2 { orientation2 = north, position2 = zero, history = [zero] }

move2 :: State2 -> Move -> State2
move2 (State2 dir pos _) (Move t s) = State2 dir' pos' (walk s dir' pos)
  where
    dir' = turn t dir
    pos' = pos .+. s *. dir'

visits :: [Move] -> History
visits = concat . map history . scanl move2 start2

initSets :: Ord a => [a] -> [Set a]
initSets = scanl (flip insert) Data.Set.empty

firstRepeated :: Ord a => [a] -> a
firstRepeated xs = head [x | (seen, x) <- zip (initSets xs) xs, member x seen]

solve2 :: Input -> Int
solve2 = norm . firstRepeated . visits

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
