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
distance (Performance _ speed dur rest) t = speed * flight_time
  where
    nsteps = t `div` (dur + rest)
    flight_time = nsteps*dur + min dur (t `mod` (dur + rest))

furtherest :: Time -> Input -> Distance
furtherest t ps = maximum [distance p t | p <- ps]

race_time :: Int
race_time = 2503

solve1 :: Input -> Int
solve1 = furtherest race_time

testInput :: String
testInput =
    "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
    \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.\n"

tests1 :: [((Int, String), Int)]
tests1 = [((1000, testInput), 1120)]

-- Part Two --

winners :: Time -> Input ->  Map Name Int
winners t ps =
    Map.fromList $ map (unit . fst) $ leastBy (Down . snd) $
        [(n, distance p t) | p@(Performance n _ _ _) <- ps]
  where
    unit x = (x, 1)

points :: Time -> Input ->  Map Name Int
points end ps = Map.unionsWith (+) [winners t ps | t <- [1..end]]

score :: Time -> Input -> Int
score t = maximum . Map.elems . points t

solve2 :: Input -> Int
solve2 = score race_time

tests2 :: [((Int, String), Int)]
tests2 = [((1000, testInput), 689)]

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (uncurry furtherest . fmap parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (uncurry score . fmap parse) tests2))
    print (solve2 input)
