module Main where

import Parser
import Utilities
import Data.Char
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Task Char]

data Dependency a = Before a a

parse :: String -> Input
parse = tasks . map (runParser dependency) . lines
  where
    dependency = Before <$ string "Step " <*> letter <*
        string " must be finished before step " <*> letter <*
        string " can begin."
    letter = satisfy isUpper

-- step with the tasks that must be done previously
type Task a = (a, Set a)

tasks :: Ord a => [Dependency a] -> [Task a]
tasks ds = Map.assocs $
    Map.unionsWith Set.union
    ([Map.singleton after (Set.singleton before) | Before before after <- ds]
    ++
    [Map.singleton before Set.empty | Before before after <- ds])

-- Part One

solve1 :: Input -> String
solve1 = schedule

-- schedule tasks respecting dependencies, breaking ties using list order
schedule :: Ord a => [Task a] -> [a]
schedule ts = case runnable ts of
    [] -> []
    (n:_) -> n : schedule (remove n ts)

-- tasks that are available for running
runnable :: Ord a => [Task a] -> [a]
runnable ts = [n | (n, ds) <- ts, Set.null ds]

-- remove task and dependencies
remove :: Ord a => a -> [Task a] -> [Task a]
remove n ts = [(x, Set.delete n ds) | (x, ds) <- ts, x /= n]

testInput :: String
testInput = "\
\Step C must be finished before step A can begin.\n\
\Step C must be finished before step F can begin.\n\
\Step A must be finished before step B can begin.\n\
\Step A must be finished before step D can begin.\n\
\Step B must be finished before step E can begin.\n\
\Step D must be finished before step E can begin.\n\
\Step F must be finished before step E can begin.\n"

tests1 :: [(String, String)]
tests1 = [(testInput, "CABDFE")]

-- Part Two

solve2 :: Input -> Int
solve2 = numSteps letterCost 5

-- state of the execution
data State a = State {
    clock :: Int, -- current time
    running :: [Job a], -- running tasks
    queue :: [Task a] -- tasks that have not yet run
    }
  deriving Show

type Job a = (a, Int) -- task with completion time

-- initial state for a list of tasks
start :: [Task a] -> State a
start ts = State 0 [] ts

numSteps :: Ord a => (a -> Int) -> Int -> [Task a] -> Int
numSteps cost nworkers =
    clock .
    until (null . running)
        (startJobs cost nworkers . completeJobs . advance) .
    startJobs cost nworkers .
    start

-- start idle workers on available tasks
startJobs :: Ord a => (a -> Int) -> Int -> State a -> State a
startJobs cost nworkers s = s {
    running = [(n, t + cost n) | n <- ready] ++ running s,
    queue = [(n, ds) | (n, ds) <- queue s, not (elem n ready)]
    }
  where
    t = clock s
    idle = nworkers - length (running s)
    ready = take idle (runnable (queue s))

-- advance clock to next completion
advance :: State a -> State a
advance s = s { clock = minimum (map snd (running s)) }

-- remove completed jobs from workers and prerequisites
completeJobs :: Ord a => State a -> State a
completeJobs s = s {
    running = ongoing,
    queue = [(n, Set.difference ds completed_set) | (n, ds) <- queue s]
    }
  where
    t = clock s
    (completed, ongoing) = partition ((== t) . snd) (running s)
    completed_set = Set.fromList (map fst completed)

letterCost :: Char -> Int
letterCost c = ord c - ord 'A' + 61

testCost :: Char -> Int
testCost = subtract 60 . letterCost

tests2 :: [(String, Int)]
tests2 = [(testInput, 15)]

main :: IO ()
main = do
    s <- readFile "input07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (numSteps testCost 2 . parse) tests2))
    print (solve2 input)
