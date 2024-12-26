module Main where

import Utilities
import Parser
import Data.Tuple
import Data.Map ((!))
import qualified Data.Map as Map

-- Input processing

type Input = (Int, Int)

parse :: String -> Input
parse s = case lines s of
    [l1, l2] -> (runParser startPos l1, runParser startPos l2)
    _ -> error "bad input"
  where
    startPos = string "Player " *> digit *> string " starting position: " *> nat

-- Part One

wrap :: Int -> Int
wrap n = (n-1) `mod` 10 + 1

data Player = Player {
    position :: !Int,
    score :: !Int
    }
    deriving (Eq, Ord, Show)

-- start a player at a given position
player :: Int -> Player
player pos = Player pos 0

-- move a player by a series of die throws
move :: [Int] -> Player -> Player
move ds (Player pos s) = Player pos' (s + pos')
  where
    pos' = wrap (pos + sum ds)

-- first player is about to move, second player just moved
-- n = number of throws of deterministic die
type State = (Player, Player, Int)

startState :: (Int, Int) -> State
startState (n1, n2) = (player n1, player n2, 0)

turn :: State -> State
turn (p1, p2, n) = (p2, move [die..die+2] p1, n+3)
  where
    die = n `mod` 100 + 1

-- player that just moved has won
finished :: State -> Bool
finished (_, p2, _) = score p2 >= 1000

result :: State -> Int
result (p1, _, n) = n * score p1

solve1 :: Input -> Int
solve1 = result . until finished turn . startState

testInput :: String
testInput = "\
    \Player 1 starting position: 4\n\
    \Player 2 starting position: 8\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 739785)]

-- Part Two

-- first player is about to move, second player just moved
type State2 = (Player, Player)

startState2 :: (Int, Int) -> State2
startState2 (n1, n2) = (player n1, player n2)

-- all states resulting from a turn
turn2 :: State2 -> [State2]
turn2 (p1, p2) = [(p2, move ds p1) | ds <- sequence (replicate 3 [1..3])]

-- player that just moved has won
finished2 :: State2 -> Bool
finished2 (_, p2) = score p2 >= 21

-- all non-winning player states
players :: [Player]
players = [Player pos s | pos <- [1..10], s <- [0..20]]

-- all states in which no player has won
states :: [State2]
states = [(p1, p2) | p1 <- players, p2 <- players]

-- memoized outcomes for each non-final state
win_counts :: State2 -> (Int, Int)
win_counts = win_memoized
  where
    -- wins after one turn
    win_memoized s = sumPairs [wins_all s' | s' <- turn2 s]
    -- outcome for a final or non-final state
    wins_all s
      | finished2 s = (1, 0)
      | otherwise = swap (winMap!s)
    -- map with lazily-calculated outcomes for each non-final state
    winMap = Map.fromList [(s, win_memoized s) | s <- states]

-- add pairs componentwise
sumPairs :: [(Int, Int)] -> (Int, Int)
sumPairs xys = (sum (map fst xys), sum (map snd xys))

solve2 :: Input -> Int
solve2 = uncurry max . win_counts . startState2

tests2 :: [(String, Int)]
tests2 = [(testInput, 444356092776315)]

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
