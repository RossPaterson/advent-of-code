module Main where

import Utilities
import Data.Char
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- Input processing

type Input = Cups
type Cups = [Cup]
type Cup = Int

parse :: String -> Input
parse = map digitToInt . filter isDigit

-- Part One

-- We have a list of distinct values arranged in a ring, and we will be
-- cutting segments from the ring and inserting them elsewhere.  To do
-- this efficiently, we need to keep track of the successor of each value.

-- link from each value to its successor in the ring
type Links a = Map a a

-- a linked ring of values with a distinguished current value in the ring
data Ring a = Ring !a !(Links a)
    deriving Show

-- ring of the list positioned at the start
initialRing :: Ord a => [a] -> Ring a
initialRing (x:xs) = Ring x (Map.fromList $ zip (x:xs) (xs ++ [x]))
initialRing [] = error "empty ring"

-- cup i minus one, wrapping around to the top if required
prevCup :: Int -> Cup -> Cup
prevCup n i = (i - 2) `mod` n + 1

-- one move:
-- Move the 3 values after the current value to just after the next value
-- below current that is not among those three values, wrapping around
-- as required, and then advance to the new next value after current.
move :: Ring Cup -> Ring Cup
move (Ring current successor) = Ring after_finish successor'
  where
    start = successor!current
    middle = successor!start
    finish = successor!middle
    after_finish = successor!finish
    picked_up = [start, middle, finish]
    dest = until (flip notElem picked_up) next_val (next_val current)
    next_val = prevCup (Map.size successor)
    successor' =
        Map.insert current after_finish $
        Map.insert dest start $
        Map.insert finish (successor!dest) $
        successor

-- the values in the ring following one
after_one :: Ring Cup -> Cups
after_one (Ring _ successor) =
    takeWhile (/= 1) $ iterate (successor!) $ successor!1

solve1 :: Input -> String
solve1 = map intToDigit . after_one . times 100 move . initialRing

testInput :: String
testInput = "389125467"

tests1 :: [(String, String)]
tests1 = [(testInput, "67384529")]

-- Part Two

solve2 :: Input -> Int
solve2 =
    product . take 2 . after_one .
        times 10000000 move . initialRing . (++ [10..1000000])

tests2 :: [(String, Int)]
tests2 = [(testInput, 149245887792)]

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    -- solve2 is slow
    -- putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
