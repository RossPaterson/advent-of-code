module Main where

import Utilities
import qualified Data.IntMap as Map

-- Input processing

type Input = [Int]

parse :: String -> Input
parse s = read $ "[" ++ s ++ "]"

-- Part One

-- state while generating the sequence
-- The strictness annotations are needed for manageable memory use.
data State = State {
    turn :: !Int, -- turn number
    output :: !Int, -- output on this turn
    -- last turn number of values seen on earlier turns
    last_seen :: !(Map.IntMap Int)
    }
    deriving Show

-- state at the last number in the input list
initState :: [Int] -> State
initState vs = State {
    turn = length vs,
    output = last vs,
    last_seen = Map.fromList (zip (init vs) [1..]) }

nextState :: State -> State
nextState (State { turn = i, output = v, last_seen = m }) = State {
    turn = i + 1,
    output = maybe 0 (i -) (Map.lookup v m),
    last_seen = Map.insert v i m }

-- The full sequence from a given starting list
-- vanEck [0] is Van Eck's sequence (https://oeis.org/A181391)
vanEck :: [Int] -> [Int]
vanEck vs = init vs ++ map output (iterate nextState (initState vs))

-- equivalent to (vanEck vs)!!(n-1), but a bit faster
vanEck_at :: Int -> [Int] -> Int
vanEck_at n vs
  | n <= length vs = vs!!(n-1)
  | otherwise = output $ until (\ s -> turn s == n) nextState $ initState vs

solve1 :: Input -> Int
solve1 = vanEck_at 2020

testInput :: String
testInput = "0,3,6"

tests1 :: [(String, Int)]
tests1 = [
    ("0,3,6", 436),
    ("1,3,2", 1),
    ("2,1,3", 10),
    ("1,2,3", 27),
    ("2,3,1", 78),
    ("3,2,1", 438),
    ("3,1,2", 1836)]

-- Part Two

solve2 :: Input -> Int
solve2 = vanEck_at 30000000

tests2 :: [(String, Int)]
tests2 = [
    ("0,3,6", 175594),
    ("1,3,2", 2578),
    ("2,1,3", 3544142),
    ("1,2,3", 261214),
    ("2,3,1", 6895259),
    ("3,2,1", 18),
    ("3,1,2", 362)]

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    -- These take a long time
    -- putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
