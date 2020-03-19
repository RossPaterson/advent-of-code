module Main where

import Utilities
import Data.Foldable
import Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq

type Input = Int

parse :: String -> Input
parse = read

-- last inserted value at right end
type State = Seq Int

initState :: State
initState = Seq.singleton 0

step :: Input -> State -> Int -> State
step incr s n = case Seq.splitAt (incr `mod` Seq.length s) s of
    (front, back) -> (back >< front) |> n

valAfter :: Int -> Input -> Int
valAfter n incr = head $ toList $ foldl (step incr) initState [1..n]

solve1 :: Input -> Int
solve1 = valAfter 2017

tests1 :: [(Input, Int)]
tests1 = [(3, 638)]

-- Part Two

-- The last number inserted (which is one less than the size of the buffer)
-- and the position at which it was inserted.
data State2 = State2 { lastInsert :: Int, position :: Int }
    deriving Show

initState2 :: State2
initState2 = State2 0 0

step2 :: Input -> State2 -> State2
step2 incr s =
    State2 { lastInsert = n, position = (position s + incr) `mod` n + 1 }
  where
    n = lastInsert s + 1

-- list of values inserted after the zero in the first n steps
valuesFollowing0 :: Int -> Input -> [Int]
valuesFollowing0 n incr =
   [lastInsert s |
       s <- take (n+1) (iterate (step2 incr) initState2), position s == 1]

solve2 :: Input -> Int
solve2 = last . valuesFollowing0 50000000

tests2 :: [((Int, Input), Int)]
tests2 = [((9, 3), 9)]

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (\ (n, incr) -> last (valuesFollowing0 n incr)) tests2))
    print (solve2 input)
