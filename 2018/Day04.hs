module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord

-- Input processing

type Input = [Shift]

-- a line in the log
type Record = (Timestamp, Event)

data Timestamp = Timestamp Int Int Int Int Int
  deriving Show

minutes :: Timestamp -> Int
minutes (Timestamp _yr _mo _dy _h m) = m

data Event = Start Int | Sleeps | Wakes
  deriving Show

parse :: String -> Input
parse = getShifts . map (runParser record) . sort . lines
  where
    record = (,) <$> timestamp <* char ' ' <*> event
    timestamp =
        Timestamp <$ char '[' <*>
        nat <* char '-' <*> nat <* char '-' <*> nat <* char ' ' <*>
        nat <* char ':' <*> nat <* char ']'
    event =
        Start <$ string "Guard #" <*> nat <* string " begins shift" <|>
        Sleeps <$ string "falls asleep" <|>
        Wakes <$ string "wakes up"

-- collected events on a single shift by a guard
data Shift = Shift Int [Nap]
  deriving Show

-- start (inclusive) and end (exclusive) times of a nap
data Nap = Nap Int Int
  deriving Show

-- collected naps for each shift
getShifts :: [Record] -> [Shift]
getShifts [] = []
getShifts ((_, Start n):rs) =
    Shift n naps : getShifts rest
  where
    (naps, rest) = getNaps rs
    getNaps ((t1, Sleeps):(t2, Wakes):rs') =
        (Nap (minutes t1) (minutes t2):naps', rest')
      where
        (naps', rest') = getNaps rs'
    getNaps rs' = ([], rs')
getShifts _ = error "malformed shift"

-- Part One

solve1 :: Input -> Int
solve1 = uncurry (*) . strategy1

-- The guard who slept most, and the minute they sleep most often
strategy1 :: [Shift] -> (Int, Int)
strategy1 gs = (g, bestMinute)
  where
    m = guardNaps gs
    g = sleepiest m
    bestMinute = head (mostCommon (concatMap asleep (m!g)))

-- All naps for each guard (combining all shifts)
guardNaps :: [Shift] -> Map Int [Nap]
guardNaps gs = Map.unionsWith (++) [Map.singleton g naps | Shift g naps <- gs]

-- The guard with the most minutes asleep
sleepiest :: Map Int [Nap] -> Int
sleepiest =
    fst . head . sortBy (comparing (Down . snd)) . map (fmap total) . Map.toList

total :: [Nap] -> Int
total = sum  . map duration

duration :: Nap -> Int
duration (Nap t1 t2) = t2 - t1

-- All the minutes in a nap
asleep :: Nap -> [Int]
asleep (Nap t1 t2) = [t1..t2-1]

testInput :: String
testInput =
    "[1518-11-01 00:00] Guard #10 begins shift\n\
    \[1518-11-01 00:05] falls asleep\n\
    \[1518-11-01 00:25] wakes up\n\
    \[1518-11-01 00:30] falls asleep\n\
    \[1518-11-01 00:55] wakes up\n\
    \[1518-11-01 23:58] Guard #99 begins shift\n\
    \[1518-11-02 00:40] falls asleep\n\
    \[1518-11-02 00:50] wakes up\n\
    \[1518-11-03 00:05] Guard #10 begins shift\n\
    \[1518-11-03 00:24] falls asleep\n\
    \[1518-11-03 00:29] wakes up\n\
    \[1518-11-04 00:02] Guard #99 begins shift\n\
    \[1518-11-04 00:36] falls asleep\n\
    \[1518-11-04 00:46] wakes up\n\
    \[1518-11-05 00:03] Guard #99 begins shift\n\
    \[1518-11-05 00:45] falls asleep\n\
    \[1518-11-05 00:55] wakes up\n"

-- In the test input, Guard #10 spent the most time asleep (50 minutes),
-- was asleep the most times at minute 24 (two times).
tests1 :: [(String, Int)]
tests1 = [(testInput, 240)]

-- Part Two

solve2 :: Input -> Int
solve2 = uncurry (*) . strategy2

-- The guard and minute most often asleep
strategy2 :: [Shift] -> (Int, Int)
strategy2 gs = head $
    mostCommon [(g, m) | Shift g naps <- gs, nap <- naps, m <- asleep nap]

-- In the test input, Guard #99 was asleep at minute 45 more than any
-- other combination (3 times).
tests2 :: [(String, Int)]
tests2 = [(testInput, 4455)]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
