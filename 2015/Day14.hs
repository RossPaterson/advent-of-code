module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map

type Name = String
type Time = Int
type Distance = Int
type Speed = Int
data Performance = Performance Name Speed Time Time
  deriving Show
type Input = [Performance]

parse :: String -> Input
parse = map (runParser statement) . lines
  where
    statement =
        Performance <$>
            name <* string " can fly " <*> nat <* string " km/s for " <*>
            nat <* string " seconds, but then must rest for " <*> nat <*
            string " seconds."
    name = some letter

distance :: Performance -> Time -> Distance
distance (Performance n speed dur rest) t = speed * flight_time
  where
    nsteps = t `div` (dur + rest)
    flight_time = nsteps*dur + min dur (t `mod` (dur + rest))

furtherest :: Time -> Input -> Distance
furtherest t ps = maximum [distance p t | p <- ps]

race_time :: Int
race_time = 2503

solve1 :: Input -> Int
solve1 = furtherest race_time

test =
    "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
    \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.\n"

-- Part Two --

winners :: Time -> Input ->  Map Name Int
winners t ps =
    Map.fromList $ map (unit . fst) $ leastBy (Down . snd) $
        [(n, distance p t) | p@(Performance n _ _ _) <- ps]
  where
    unit x = (x, 1)

points :: Time -> Input ->  Map Name Int
points end ps = Map.unionsWith (+) [winners t ps | t <- [1..end]]

solve2 :: Input -> Int
solve2 = maximum . Map.elems . points race_time

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
