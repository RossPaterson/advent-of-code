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

type Input = Prerequisites Char

data Dependency a = Before a a

parse :: String -> Input
parse = prerequisites . map (runParser dependency) . lines
  where
    dependency = Before <$ string "Step " <*> capital <*
        string " must be finished before step " <*> capital <*
        string " can begin."
    capital = satisfy isUpper

-- set of prerequisites for each task
type Prerequisites a = Map a (Set a)

prerequisites :: Ord a => [Dependency a] -> Prerequisites a
prerequisites ds = Map.unionsWith Set.union
    ([Map.singleton after (Set.singleton before) | Before before after <- ds]
    ++
    [Map.singleton before Set.empty | Before before _after <- ds])

-- Part One

solve1 :: Input -> String
solve1 ds = schedule ds (Map.keysSet ds)

-- schedule tasks respecting dependencies, breaking ties using list order
schedule :: Ord a => Prerequisites a -> Set a -> [a]
schedule ds ts = case runnable ds ts of
    [] -> []
    (t:_) -> t : schedule ds (Set.delete t ts)

-- tasks that are available for running
runnable :: Ord a => Prerequisites a -> Set a -> [a]
runnable ds ts = [t | t <- Set.toList ts, Set.disjoint (ds!t) ts]

testInput :: String
testInput =
    "Step C must be finished before step A can begin.\n\
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
    queue :: Set a, -- tasks that have not yet started
    running :: [Job a], -- running tasks
    completed :: Set a -- tasks that have finished
    }
  deriving Show

type Job a = (a, Int) -- task with completion time

numSteps :: Ord a => (a -> Int) -> Int -> Prerequisites a -> Int
numSteps cost nworkers ds =
    clock $
    until (null . running)
        (startJobs cost nworkers ds . completeJobs . advance) $
    startJobs cost nworkers ds $
    start ds

-- initial state for a list of tasks
start :: Ord a => Prerequisites a -> State a
start ds = State {
    clock = 0,
    queue = Map.keysSet ds,
    running = [],
    completed = Set.empty
    }

-- start idle workers on available tasks
startJobs :: Ord a => (a -> Int) -> Int -> Prerequisites a -> State a -> State a
startJobs cost nworkers ds s = s {
    running = [(n, now + cost n) | n <- ready] ++ running s,
    queue = Set.difference (queue s) (Set.fromList ready)
    }
  where
    now = clock s
    idle = nworkers - length (running s)
    ready = take idle
        [t | t <- Set.toList (queue s), Set.isSubsetOf (ds!t) (completed s)]

-- advance clock to next completion
advance :: State a -> State a
advance s = s { clock = minimum (map snd (running s)) }

-- remove completed jobs from workers and prerequisites
completeJobs :: Ord a => State a -> State a
completeJobs s = s {
    running = ongoing,
    completed = Set.union (completed s) (Set.fromList (map fst completing))
    }
  where
    t = clock s
    (completing, ongoing) = partition ((== t) . snd) (running s)

letterCost :: Char -> Int
letterCost c = ord c - ord 'A' + 61

testCost :: Char -> Int
testCost = subtract 60 . letterCost

tests2 :: [(String, Int)]
tests2 = [(testInput, 15)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (numSteps testCost 2 . parse) tests2))
    print (solve2 input)
